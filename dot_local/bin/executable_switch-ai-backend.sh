#!/usr/bin/env bash
# switch-ai-backend.sh {omni|instruct|image} [opts]
#
# Swap which backend holds the single V100 (32 GB). Only ONE workload holds it
# at a time:
#   * instruct -> llama.cpp CUDA backend  (:8081, ornith)  [llama-cuda.service, boot default]
#   * omni     -> vLLM-Omni               (:8091, Qwen2.5-Omni-7B by default)
#   * image    -> sd.cpp (stable-diffusion.cpp) sd-cli, runs a batch file
#
# Olla (:40114) routes each request by model id to whichever backend is live
# and picks up the change on its discovery refresh. This script does NOT restart
# Olla — its ExecStartPre requires :8081 reachable, so restarting while vLLM-Omni
# or sd.cpp holds the V100 (no :8081) would fail; Olla's periodic refresh is enough.
#
# sudo (NOPASSWD drop-in, /usr/bin/systemctl --system) is used ONLY to manage the
# llama-cuda system unit. vLLM-Omni is a plain background process started by
# restart-vllm-omni.sh. Reaping the llama backend does NOT umount the shared HF
# cache bind mount — it must stay mounted for vLLM-Omni / sd.cpp.
set -uo pipefail

RESTART_OMNI="$HOME/.local/bin/restart-vllm-omni.sh"
SUDO="/usr/bin/systemctl --system"
SD_CLI="/home/jan/projs/stable-diffusion.cpp/build/bin/sd-cli"
DEFAULT_DEADLINE=300   # seconds the image mode holds the V100 before auto-switch-back

reap() {
  # port-bound reap of a named binary family (llama / api_server) on $1.
  local family="$1" port="$2" pid cmd
  for pid in $(pgrep -x "$family" 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null)
    case "$cmd" in *"$port"*)
      echo "switch: reaping $family pid $pid on :$port"
      kill "$pid" 2>/dev/null
      for _ in $(seq 1 15); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
      kill -0 "$pid" 2>/dev/null && { kill -9 "$pid" 2>/dev/null; }
      ;;
    esac
  done
}

wait_ready() {
  local port="$1" i
  for i in $(seq 1 90); do
    curl -fsS "http://127.0.0.1:$port/v1/models" >/dev/null 2>&1 && return 0
    sleep 2
  done
  return 1
}

# is_active BACKEND: "llama" | "omni" | "" — which text backend currently holds
# the V100, so image mode can restore it afterwards.
is_active() {
  if systemctl --system is-active --quiet llama-cuda 2>/dev/null; then
    echo llama
  elif curl -fsS "http://127.0.0.1:8091/v1/models" >/dev/null 2>&1; then
    echo omni
  else
    echo ""
  fi
}

restore() {
  local who="$1"
  case "$who" in
    llama)
      "$SUDO" start llama-cuda
      if wait_ready 8081; then echo "switch: llama.cpp backend healthy on :8081.";
      else echo "switch: llama.cpp not healthy on :8081." >&2; exit 1; fi
      ;;
    omni)
      "$RESTART_OMNI" start
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

ORDER = ["model", "llm", "vae", "vae-format", "tokenizer",
         "prompt", "seed", "steps", "guidance", "cfg-scale",
         "width", "height", "flow-shift", "clip", "negative-prompt", "output"]

def argv_for(call):
    out = dict(shared)
    out.update({k: v for k, v in call.items() if k != "output"})  # call overrides shared
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
  $0 omni [MODEL]             -> vLLM-Omni (:8091)
  $0 image --batch FILE [--deadline SECONDS]
                              -> sd.cpp batch on the V100 (default deadline 300s)

image batch file (JSON): shared top-level flags + a 'calls' array; each call
sets its prompt/params and MUST define 'output' (the result location):
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

MODEL="Qwen/Qwen2.5-Omni-7B"
case "${1:-instruct}" in
  omni)
    [ "${2:-}" != "" ] && MODEL="$2"
    echo "== switching to vLLM-Omni (:8091, $MODEL) =="
    reap llama 8081                                   # free VRAM (no umount of shared cache)
    "$SUDO" stop llama-cuda                           # reset the unit so it is not 'active'
    "$RESTART_OMNI" start "$MODEL"
    exit $?
    ;;
  instruct)
    echo "== switching to llama.cpp (:8081, ornith) =="
    reap api_server 8091                              # free VRAM from vLLM-Omni
    "$RESTART_OMNI" stop
    "$SUDO" start llama-cuda                          # relaunch the llama backend on :8081
    if wait_ready 8081; then
      echo "switch: llama.cpp backend healthy on :8081."
    else
      echo "switch: llama.cpp not healthy on :8081." >&2; exit 1
    fi
    ;;
  image)
    BATCH=""
    DEADLINE="$DEFAULT_DEADLINE"
    while [ $# -gt 0 ]; do
      case "$1" in
        --batch) BATCH="${2:-}"; shift 2 ;;
        --deadline) DEADLINE="${2:-}"; shift 2 ;;
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

    WHO="$(is_active)"
    echo "== switching to sd.cpp image mode (:V100, deadline ${DEADLINE}s) =="
    reap llama 8081                                   # free V100 from llama.cpp
    reap api_server 8091                              # free V100 from vLLM-Omni
    "$RESTART_OMNI" stop                              # defensive: make sure omni is down
    if [ "$(nvidia-smi --query-gpu=memory.used --format=csv,noheader 2>/dev/null | tr -dc 0-9)" != 0 ] 2>/dev/null; then
      echo "switch: WARNING V100 still shows memory used — a non-text holder may hold it" >&2
    fi

    RESULTS="$(run_image_batch "$BATCH" "$DEADLINE")"
    echo "$RESULTS"
    BATCH_RC=$?

    echo "== image batch done; restoring ${WHO:-none} =="
    restore "$WHO"
    exit "${BATCH_RC}"
    ;;
  *)
    usage; exit 2 ;;
esac
