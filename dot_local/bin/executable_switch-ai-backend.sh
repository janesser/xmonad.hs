#!/usr/bin/env bash
# switch-ai-backend.sh {instruct|image} [opts]
#
# Swap which backend holds the single V100 (32 GB). Only ONE workload holds it
# at a time:
#   * instruct -> llama.cpp CUDA backend  (:8081, ornith)  [llama-cuda.service, boot default]
#   * image    -> sd.cpp (stable-diffusion.cpp) sd-cli, runs a batch file
#
# Olla (:40114) routes each request by model id to whichever backend is live
# and picks up the change on its discovery refresh. This script does NOT restart
# Olla — its ExecStartPre requires :8081 reachable, so restarting while vLLM-Omni
# or sd.cpp holds the V100 (no :8081) would fail; Olla's periodic refresh is enough.
#
# sudo (NOPASSWD drop-in, /usr/bin/systemctl --system) is used ONLY to manage the
# llama-cuda system unit. The only thing we do to a backend is STOP it — a plain
# `systemctl stop llama-cuda` — no manual signal scanning. Stopping the llama-cuda
# unit does NOT umount the shared HF cache bind mount — it must stay mounted for
# sd.cpp.
set -uo pipefail

SUDO=(sudo systemctl)        # array: invoked as "${SUDO[@]}" so 'sudo systemctl' splits into 2 args
SD_CLI="/home/jan/projs/stable-diffusion.cpp/build/bin/sd-cli"
DEFAULT_DEADLINE=300   # seconds the image mode holds the V100 before auto-switch-back

wait_ready() {
  local port="$1" i
  # $port is caller-supplied (never trust it in a URL): gate it to numeric
  # 1..65535 so it cannot become a tainted URL component.
  case "$port" in
    ''|*[!0-9]*) echo "switch: wait_ready: port '$port' is not numeric" >&2; return 1 ;;
    *[0-9][0-9][0-9][0-9][0-9][0-9]*) echo "switch: wait_ready: port '$port' exceeds 65535" >&2; return 1 ;;
  esac
  for i in $(seq 1 90); do
    # foxguard: ignore[bash/taint-ssrf]
    # $port is gated to numeric 1..65535 by the case guard above, so the URL
    # host is fixed and the port cannot be tampered with.
    curl -fsS "http://127.0.0.1:$port/v1/models" >/dev/null 2>&1 && return 0
    sleep 2
  done
  return 1
}

# is_active BACKEND: "llama" | "" — which text backend currently holds
# the V100, so image mode can restore it afterwards. vLLM-Omni (:8091) is
# retired — the V100 now hosts the 27B on :8081 or an sd.cpp batch.
is_active() {
  if systemctl --system is-active --quiet llama-cuda 2>/dev/null; then
    echo llama
  else
    echo ""
  fi
}

restore() {
  local who="$1"
  case "$who" in
    llama)
      "${SUDO[@]}" restart llama-cuda   # NOT start: the unit is RemainAfterExit 'active (exited)', so
                                      #   start is a no-op and won't relaunch the reaped orphan backend
      if wait_ready 8081; then echo "switch: llama.cpp backend healthy on :8081.";
      else echo "switch: llama.cpp not healthy on :8081." >&2; exit 1; fi
      ;;
    *)
      echo "switch: no prior text backend recorded; leaving V100 free."
      ;;
  esac
}

# run_image_batch BATCH DEADLINE: run the sd.cpp batch file. DEADLINE seconds is
# the hard cap for the WHOLE batch (per-call graceful SIGTERM at the deadline);
# an early-complete batch returns immediately. Prints a JSON results array.
run_image_batch() {
  python3 - "$1" "$2" <<'PY'
import json, os, signal, subprocess, sys, threading, time

batch_path, deadline = sys.argv[1], float(sys.argv[2])
SD = os.environ.get("SD_CLI_BIN", "/home/jan/projs/stable-diffusion.cpp/build/bin/sd-cli")

with open(batch_path) as f:
    spec = json.load(f)

model = spec.get("model")
if not model:
    print("image-batch: top-level 'model' (diffusion GGUF) is required in the batch")
    sys.exit(2)
shared = {k: spec[k] for k in ("model", "llm", "vae", "tokenizer", "vae-format") if spec.get(k)}
calls = spec.get("calls") or []
if not calls:
    print("image-batch: no 'calls' in batch")
    sys.exit(2)

ORDER = ["diffusion-model", "llm", "vae", "vae-format", "tokenizer",
         "prompt", "seed", "steps", "guidance", "cfg-scale",
         "width", "height", "flow-shift", "negative-prompt", "output"]

def argv_for(call):
    out = dict(shared)
    out.update({k: v for k, v in call.items() if k != "output"})  # call overrides shared
    # standalone diffusion GGUF -> --diffusion-model (NOT --model; see leejet sd.cpp #2122)
    out["diffusion-model"] = out.pop("model")
    output = call.get("output")
    if not output:
        raise SystemExit("image-batch: each call must define 'output' (result location)")
    argv = [SD]
    for k in ORDER:
        if k in out:
            argv += [f"--{k}", str(out[k])]
    # any flags not in the known order list are passed through verbatim
    for k in out:
        if k not in ORDER:
            argv += [f"--{k}", str(out[k])]
    argv += ["--output", str(output)]   # output always last
    return argv

start = time.time()
results = []

def run_one(argv, index):
    deadline_ms = deadline - (time.time() - start)
    proc = subprocess.Popen(argv)
    if deadline_ms > 0:
        def watchdog():
            time.sleep(deadline_ms)
            if proc.poll() is None:
                print(f"image-batch: deadline hit; SIGTERM pid {proc.pid} (call {index})")
                proc.send_signal(signal.SIGTERM)
                try: proc.wait(timeout=10)
                except Exception: proc.kill()
        threading.Thread(target=watchdog, daemon=True).start()
    return proc.wait()

for i, call in enumerate(calls, 1):
    if deadline and time.time() - start >= deadline:
        print(f"image-batch: deadline reached before call {i}; stopping batch")
        break
    argv = argv_for(call)
    output = call.get("output")
    print(f"image-batch: call {i}/{len(calls)} -> {output}")
    t0 = time.time()
    code = run_one(argv, i)
    dt = time.time() - t0
    ok = code == 0 and bool(output) and os.path.isfile(output) and os.path.getsize(output) > 0
    results.append({"call": i, "output": output, "exit": code, "seconds": round(dt, 1), "ok": ok})
    print(f"image-batch: call {i} exit={code} {dt:.1f}s ok={ok}")
    if not ok:
        print(f"image-batch: call {i} failed (exit {code}); stopping batch")
        break

print(json.dumps(results, indent=2))
PY
}

usage() {
  cat >&2 <<EOF
usage:
  $0 instruct                 -> llama.cpp (:8081, ornith)
  $0 image --batch FILE [--deadline SECONDS] [--no-evict]
                              -> sd.cpp batch on the V100 (default deadline 300s)
                              --no-evict: do NOT stop/restore the text backends
                              (card2-swap.sh uses this for card-2 batches)

image batch file (JSON): shared top-level flags + a 'calls' array; each call
sets its prompt/params and MUST define 'output' (the result location). The
top-level 'model' key is the standalone diffusion GGUF and is passed as
--diffusion-model (NOT --model) so upstream sd.cpp loads the checkpoint.
  {
    "model":   "/path/to/diffusion.gguf",
    "llm":     "/path/to/text_encoder.gguf",
    "vae":     "/path/to/vae.safetensors",
    "vae-format": "flux2",
    "calls": [
      {"prompt": "...", "seed": 42, "steps": 20, "guidance": 4,
       "width": 512, "height": 512, "output": "/path/out1.png"},
      {"prompt": "...", "seed": 7,  "steps": 20, "guidance": 4,
       "width": 512, "height": 512, "output": "/path/out2.png"}
    ]
  }
EOF
}

case "${1:-instruct}" in
  instruct)
    echo "== switching to llama.cpp (:8081, ornith) =="
    "${SUDO[@]}" restart llama-cuda                      # relaunch the llama backend on :8081 (see restore: start==no-op on a stale-active oneshot)
    if wait_ready 8081; then
      echo "switch: llama.cpp backend healthy on :8081."
    else
      echo "switch: llama.cpp not healthy on :8081." >&2; exit 1
    fi
    ;;
  image)
    shift    # consume the 'image' subcommand token before parsing its options
    BATCH=""
    DEADLINE="$DEFAULT_DEADLINE"
    NO_EVICT=0
    while [ $# -gt 0 ]; do
      case "$1" in
        --batch) BATCH="${2:-}"; shift 2 ;;
        --deadline) DEADLINE="${2:-}"; shift 2 ;;
        --no-evict) NO_EVICT=1; shift ;;
        *) echo "image: unknown option: $1" >&2; usage; exit 2 ;;
      esac
    done
    if [ -z "$BATCH" ]; then echo "image: --batch FILE is required" >&2; usage; exit 2; fi
    if [ ! -f "$BATCH" ]; then echo "image: batch file not found: $BATCH" >&2; exit 2; fi
    if ! python3 -c 'import json,sys; json.load(open(sys.argv[1]))' "$BATCH" 2>/dev/null; then
      echo "image: batch file is not valid JSON" >&2; exit 2
    fi
    if ! command -v nvidia-smi >/dev/null 2>&1; then
      echo "image: nvidia-smi not found — not an NVIDIA box; refusing" >&2; exit 1
    fi
    [ -x "$SD_CLI" ] || { echo "image: sd-cli not found at $SD_CLI" >&2; exit 1; }

    WHO=""
    if [ "${NO_EVICT}" -eq 1 ]; then
      echo "== sd.cpp image mode, --no-evict (shared card, deadline ${DEADLINE}s) =="
      echo "switch: --no-evict: leaving the llama-cuda backend on :8081 running."
    else
      WHO="$(is_active)"
      echo "== switching to sd.cpp image mode (:V100, deadline ${DEADLINE}s) =="
      "${SUDO[@]}" stop llama-cuda                      # stop the llama backend; frees the V100
      if [ "$(nvidia-smi --query-gpu=memory.used --format=csv,noheader 2>/dev/null | tr -dc 0-9)" != 0 ] 2>/dev/null; then
        echo "switch: WARNING V100 still shows memory used — a non-text holder may hold it" >&2
      fi
    fi

    RESULTS="$(run_image_batch "$BATCH" "$DEADLINE")"
    BATCH_RC=$?             # capture BEFORE the echo, else it records echo's exit (0)
    echo "$RESULTS"

    if [ "${NO_EVICT}" -eq 1 ]; then
      echo "== image batch done (--no-evict: nothing to restore) =="
    else
      echo "== image batch done; restoring ${WHO:-none} =="
      restore "$WHO"
    fi
    exit "${BATCH_RC}"
    ;;
  *)
    usage; exit 2 ;;
esac
