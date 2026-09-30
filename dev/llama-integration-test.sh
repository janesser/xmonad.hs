#!/usr/bin/env bash
# llama-integration-test.sh — pre-flight integration harness for the
# llama.cpp  ->  Olla  ->  pi-agent stack.
#
# WHY
#   Before a llama.cpp change lands in chezmoi (and therefore on the productive
#   runtime), this harness builds that exact version in an ISOLATED workspace,
#   serves it on a throwaway port, stands up a throwaway Olla in front of it,
#   points a throwaway pi request at it, and probes end-to-end with a "say hi"
#   prompt. Nothing touches your real ~/.config/olla, ~/.local/bin launchers,
#   systemd units, or ~/.pi — every moving part runs under $IT_WORK with its
#   own ports and config, and is killed on exit.
#
# FLOW
#   build(<ref>) -> serve(:llama) -> ollama(:test config) -> pi --print
#
# USAGE
#   ./llama-integration-test.sh                      # CPU build + small model, fast
#   ./llama-integration-test.sh --backend cuda       # real CUDA build (needs GPU + sudo)
#   ./llama-integration-test.sh --ref v0.5.0         # pinned llama.cpp ref / tag
#   ./llama-integration-test.sh --model /path/to/big.gguf
#   ./llama-integration-test.sh --only ollama        # stage 2 only (reuses build+serve)
#   ./llama-integration-test.sh --only pi            # stage 3 only (reuses build+serve+ollama)
#
# ENV OVERRIDES: IT_WORK, LLAMA_REF, IT_BACKEND, IT_MODEL, IT_LLAMA_PORT,
#   IT_OLLA_PORT, IT_CTX_SIZE, IT_PROMPT, OLLA_BIN, NCPUS.
#
# The build needs build tooling; CUDA additionally needs sudo apt for a handful
# of dev packages on a fresh box (idempotent). This script is a dev tool, NOT a
# chezmoi run script — `dev/` is a top-level, non-rendered path, so `cz apply`
# never touches it.

set -uo pipefail

# --------------------------------------------------------------------------
# Knobs
# --------------------------------------------------------------------------
IT_WORK="${IT_WORK:-$HOME/.local/share/llama-integration-test}"
LLAMA_REF="${LLAMA_REF:-v0.5.0}"                 # git ref / tag of llama.cpp to build
BACKEND="${IT_BACKEND:-cpu}"                      # cpu | cuda
MODEL="${IT_MODEL:-}"                             # override -> path to a .gguf
LLAMA_PORT="${IT_LLAMA_PORT:-8091}"               # test llama-server (prod :8081)
OLLA_PORT="${IT_OLLA_PORT:-40115}"                # test Olla           (prod :40114)
CTX_SIZE="${IT_CTX_SIZE:-8192}"                   # ctx window: pi's default coding-assistant system prompt is ~6k tokens (so 4096 is too small); kept modest to stay fast/lean on CPU
OLLA_BASE_PATH="${IT_OLLA_BASE_PATH:-/olla/openai-compatible}"  # Olla OpenAI proxy base path
PROMPT="${IT_PROMPT:-Say hi in one short sentence.}"
ONLY="${IT_ONLY:-all}"                            # all | build | serve | ollama | pi
OLLASRC="${OLLASRC:-$HOME/.local/share/mise/installs/github-thushan-olla/latest/olla}"

NCPUS="${NCPUS:-$(nproc 2>/dev/null || echo 4)}"
LLAMA_SRC="$IT_WORK/llama.cpp"                    # persistent test clone (reused)
LLIB_BIN="$LLAMA_SRC/build_${BACKEND}/bin/llama-server"  # build dir == backend

# Small, already-cached model so a smoke run is fast. Default the path to a
# symlink so llama.cpp sees a clean .gguf name; override with --model.
ANTARES_BLOB="$HOME/.cache/huggingface/hub/models--DevQuasar--fdtn-ai.antares-1b-GGUF/blobs/1f4d922bbf2d317944185ff1155feb8b9dcfa48d37617fdeaec56e9c2af54b1f"
[ -n "$MODEL" ] || MODEL="$IT_WORK/models/antares-1b.gguf"

LLAMA_LOG="$IT_WORK/log/llama-test.log"
OLLA_LOG="$IT_WORK/log/ollama-test.log"
TEST_CONFIG="$IT_WORK/olla-test.yaml"
# pi pins its Olla baseUrl in this settings file; the harness swaps it to the
# test Olla for the pi probe and always restores the original (backup kept too).
PI_OLA_SETTINGS="$HOME/.pi/olla/settings.json"
SETTINGS_BACKUP="$IT_WORK/log/settings-backup.json"

# Background PIDs (killed by the cleanup trap).
LLAMA_PID=""
OLLA_PID=""
KEEP="${IT_KEEP:-0}"          # keep==1 => leave running processes alive (debug/only-mode)

# --------------------------------------------------------------------------
# Logging / helpers
# --------------------------------------------------------------------------
ts()   { date +%H:%M:%S; }
log()  { echo "$(ts) [itest] $*"; }
err()  { echo "$(ts) [itest] ERROR: $*" >&2; }

free_port() {  # free_port <port> -> 0 if nothing is listening
  local p="$1"
  # ss prints a header and returns 0 even with no match, so inspect output:
  if ss -ltnH "sport = :$p" 2>/dev/null | grep -q .; then
    return 1
  fi
  return 0
}

wait_ready() {  # wait_ready <url> <label> <tries> <secs>
  local url="$1" label="$2" tries="${3:-60}" secs="${4:-2}" i
  for ((i=1; i<=tries; i++)); do
    if curl -fsS "$url" >/dev/null 2>&1; then return 0; fi
    sleep "$secs"
  done
  err "$label did not become ready after $((tries*secs))s (last hit: $url)"
  return 1
}

# wait_models <base-url> <label> <tries> <secs>
# Like wait_ready, but succeeds only when Olla actually reports >=1 model. HTTP
# 200 with an empty {"data":[]} means Olla is up but has not yet discovered the
# backend's model — autodetect would then register an empty ollama provider and
# pi's --model would find nothing. So we parse the data array length.
# base-url is the Olla base WITHOUT the trailing /v1/models (i.e. $OLLA_BASE_URL).
wait_models() {
  local base="$1" label="$2" tries="${3:-60}" secs="${4:-2}" i body n
  for ((i=1; i<=tries; i++)); do
    body=$(curl -fsS "$base/v1/models" 2>/dev/null)
    n=$(printf '%s' "$body" | python3 -c 'import sys,json;
try:
    print(len(json.load(sys.stdin).get("data",[])))
except Exception:
    print(0)' 2>/dev/null)
    # Ready only when the array reports >=1 model. Empty (curl/JSON failure)
    # or "0" both mean "not ready yet".
    if [ -n "$n" ] && [ "$n" -gt 0 ] 2>/dev/null; then return 0; fi
    sleep "$secs"
  done
  err "$label did not report any model after $((tries*secs))s"
  return 1
}

cleanup() {
  local rc=$?
  if [ "$KEEP" = 1 ]; then
    log "KEEP=1: leaving test backend (:$LLAMA_PORT) and test Olla (:$OLLA_PORT) running."
    exit "$rc"
  fi
  # Safety net: make sure the production Olla settings are restored even if we
  # were interrupted mid-probe (do_pi also restores explicitly).
  if [ -s "$SETTINGS_BACKUP" ] && ! cmp -s "$SETTINGS_BACKUP" "$PI_OLA_SETTINGS"; then
    cp "$SETTINGS_BACKUP" "$PI_OLA_SETTINGS" 2>/dev/null && \
      log "restored $PI_OLA_SETTINGS from backup"
  fi
  [ -n "$OLLA_PID" ] && kill "$OLLA_PID" 2>/dev/null
  [ -n "$LLAMA_PID" ] && kill "$LLAMA_PID" 2>/dev/null
  if [ $rc -ne 0 ]; then
    err "harness exited rc=$rc — logs: $LLAMA_LOG  $OLLA_LOG"
  else
    log "cleaned up test backend (:$LLAMA_PORT) and test Olla (:$OLLA_PORT)."
  fi
  exit "$rc"
}
trap cleanup EXIT

# --------------------------------------------------------------------------
# Model setup
# --------------------------------------------------------------------------
ensure_model() {
  if [ -e "$MODEL" ]; then return 0; fi
  mkdir -p "$IT_WORK/models"
  if [ -e "$ANTARES_BLOB" ]; then
    ln -sfn "$ANTARES_BLOB" "$MODEL"
  else
    err "no default model at $ANTARES_BLOB; pass --model <path.to.gguf>"
    exit 1
  fi
}

# --------------------------------------------------------------------------
# Build llama.cpp (isolated clone + per-ref build dir — prod clone never touched)
# --------------------------------------------------------------------------
fetch_llama_cpp() {
  local ref="$1"
  if [ ! -d "$LLAMA_SRC/.git" ]; then
    log "cloning llama.cpp -> $LLAMA_SRC"
    git clone --quiet https://github.com/ggml-org/llama.cpp "$LLAMA_SRC"
  fi
  if ! ( cd "$LLAMA_SRC" && git fetch --all --prune && git checkout --quiet "$ref" ); then
    err "could not fetch/checkout '$ref' in $LLAMA_SRC"
    exit 1
  fi
  log "llama.cpp at $LLAMA_SRC ($(cd "$LLAMA_SRC" && git rev-parse --short HEAD) = $ref)"
}

build_cpu() {
  local build="$LLAMA_SRC/build_${BACKEND}"
  for t in cmake c++; do command -v "$t" >/dev/null 2>&1 || { err "missing build tool: $t"; exit 1; }; done
  log "building llama.cpp (CPU) -> $build"
  cmake -B "$build" "$LLAMA_SRC" \
    -DGGML_CCACHE=ON -DGGML_NATIVE=ON \
    -DLLAMA_BUILD_TESTS=OFF -DLLAMA_BUILD_EXAMPLES=OFF -DLLAMA_BUILD_SERVER=ON
  cmake --build "$build" --config Release -j "$NCPUS"
}

build_cuda() {
  local build="$LLAMA_SRC/build_${BACKEND}"
  for t in cmake c++; do command -v "$t" >/dev/null 2>&1 || { err "missing build tool: $t"; exit 1; }; done
  log "installing CUDA build deps (sudo)..."
  sudo apt-get update -qq
  sudo apt-get install -y glslang-dev glslc spirv-headers libssl-dev libnccl-dev ccache nvidia-cuda-toolkit
  log "building llama.cpp (CUDA) -> $build"
  cmake -B "$build" "$LLAMA_SRC" \
    -DGGML_CCACHE=ON -DGGML_CUDA=ON \
    -DLLAMA_BUILD_TESTS=OFF -DLLAMA_BUILD_EXAMPLES=OFF -DLLAMA_BUILD_SERVER=ON
  cmake --build "$build" --config Release -j "$NCPUS"
}

do_build() {
  case "$BACKEND" in
    cpu)  build_cpu  ;;
    cuda) build_cuda ;;
    *)    err "unknown --backend $BACKEND (use cpu|cuda)"; exit 1 ;;
  esac
  [ -x "$LLIB_BIN" ] || { err "build did not produce $LLIB_BIN"; exit 1; }
  log "backend binary ready: $LLIB_BIN"
}

# --------------------------------------------------------------------------
# Stage: serve
# --------------------------------------------------------------------------
do_serve() {
  free_port "$LLAMA_PORT" || { err ":$LLAMA_PORT already in use"; exit 1; }
  mkdir -p "$(dirname "$LLAMA_LOG")"
  log "serving $MODEL on 127.0.0.1:$LLAMA_PORT"
  "$LLIB_BIN" \
    --host 127.0.0.1 --port "$LLAMA_PORT" \
    --model "$MODEL" \
    --ctx-size "$CTX_SIZE" --offline --parallel 1 --no-warmup --no-ui \
    --log-file "$LLAMA_LOG" >/dev/null 2>&1 &
  LLAMA_PID=$!
  # Wait for the model to be *loaded*, not just for the server to accept
  # connections. llama.cpp answers HTTP 200 on /v1/models before the model is
  # loaded (empty data array). Olla only discovers the model once the backend
  # actually serves it, so warming the backend here first removes the
  # cold-backend race that otherwise makes Olla's discovery flaky.
  wait_models "http://127.0.0.1:$LLAMA_PORT" "llama-server" 180 2 || exit 1
  log "llama-server serving $MODEL on :$LLAMA_PORT (pid $LLAMA_PID)."
}

# --------------------------------------------------------------------------
# Stage: Olla (test config in the isolated workspace)
# --------------------------------------------------------------------------
do_ollama() {
  free_port "$OLLA_PORT" || { err ":$OLLA_PORT already in use"; exit 1; }
  cat > "$TEST_CONFIG" <<EOF
# Generated by llama-integration-test.sh — throwaway, isolated from prod.
server:
  host: "127.0.0.1"
  port: $OLLA_PORT
discovery:
  type: "static"
  static:
    endpoints:
      - url: "http://127.0.0.1:$LLAMA_PORT"   # test llama.cpp server
        name: "test-llamacpp-${BACKEND}"
        type: "openai-compatible"
        priority: 100
EOF
  log "writing test Olla config -> $TEST_CONFIG"
  if ! "$OLLASRC" -validate-config -config "$TEST_CONFIG" >/dev/null 2>&1; then
    err "test Olla config failed validation:"
    "$OLLASRC" -validate-config -config "$TEST_CONFIG" 2>&1 | sed 's/^/    /'
    exit 1
  fi
  log "starting test Olla on 127.0.0.1:$OLLA_PORT"
  "$OLLASRC" -config "$TEST_CONFIG" >"$OLLA_LOG" 2>&1 &
  OLLA_PID=$!
  # Olla lists a model once the backend is discovered. Olla exposes its OpenAI
  # proxy under /olla/openai-compatible/, so the models endpoint is that base
  # plus /v1/models. Wait for a *non-empty* list — HTTP 200 with empty data means
  # Olla is up but has not discovered the model yet, which would register an
  # empty ollama provider and make pi's --model miss.
  OLLA_BASE_URL="http://127.0.0.1:$OLLA_PORT$OLLA_BASE_PATH"
  wait_models "$OLLA_BASE_URL" "Olla" 90 2 || exit 1
  log "Olla answering on :$OLLA_PORT (pid $OLLA_PID)."
}

# --------------------------------------------------------------------------
# Stage: pi --print (non-interactive one-shot) against test Olla
# --------------------------------------------------------------------------
do_pi() {
  # Discover the exact model id Olla exposes (pi matches on it). Olla exposes
  # its OpenAI proxy under /olla/openai-compatible/, so the models endpoint is
  # that base plus /v1/models.
  local model_id OLLA_BASE_URL
  OLLA_BASE_URL="http://127.0.0.1:$OLLA_PORT$OLLA_BASE_PATH"
  model_id=$(curl -fsS "$OLLA_BASE_URL/v1/models" 2>/dev/null \
    | python3 -c 'import sys,json;
d=json.load(sys.stdin);
print(d["data"][0]["id"] if d.get("data") else "")' 2>/dev/null)
  if [ -z "$model_id" ]; then
    err "Olla did not expose any model on :$OLLA_PORT (/v1/models empty)"
    sed 's/^/    /' "$OLLA_LOG" 2>/dev/null || true
    exit 1
  fi
  log "pi will request model '$model_id' via Olla on :$OLLA_PORT"

  # pi pins its Olla baseUrl in a settings file and this pi build honors that
  # file OVER the OLL_BASE_URL env var, so redirect pi to the test Olla by
  # temporarily rewriting the settings file. Back up the original and restore
  # it unconditionally (cleanup() also restores as a safety net).
  local original
  cp -p "$PI_OLA_SETTINGS" "$SETTINGS_BACKUP" 2>/dev/null || true
  original=$(cat "$PI_OLA_SETTINGS" 2>/dev/null)
  printf '{\n  "baseUrl": "%s"\n}\n' "$OLLA_BASE_URL" > "$PI_OLA_SETTINGS"

  OLLA_BASE_URL="$OLLA_BASE_URL" pi --print --no-tools --no-session --model "$model_id" "$PROMPT" \
    | tee "$IT_WORK/log/pi-out.txt"
  local pi_rc=${PIPESTATUS[0]}

  # Restore the production settings no matter what happened above.
  if [ -n "$original" ]; then
    printf '%s' "$original" > "$PI_OLA_SETTINGS"
  else
    rm -f "$PI_OLA_SETTINGS"
  fi
  echo

  if [ "$pi_rc" -ne 0 ]; then
    err "pi exited with code $pi_rc (response, if any, in $IT_WORK/log/pi-out.txt)"
    exit 1
  fi
  if [ ! -s "$IT_WORK/log/pi-out.txt" ]; then
    err "pi produced no response (empty) — Olla/pi wiring or model id is wrong"
    exit 1
  fi
  log "full pi response saved to $IT_WORK/log/pi-out.txt"
}

# --------------------------------------------------------------------------
# Usage
# --------------------------------------------------------------------------
usage() {
  sed -n '2,40p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

while [ $# -gt 0 ]; do
  case "$1" in
    -h|--help)    usage 0 ;;
    --backend)    BACKEND="$2"; shift 2 ;;
    --ref)        LLAMA_REF="$2"; shift 2 ;;
    --model)      MODEL="$2"; shift 2 ;;
    --llama-port) LLAMA_PORT="$2"; shift 2 ;;
    --olla-port)  OLLA_PORT="$2"; shift 2 ;;
    --prompt)     PROMPT="$2"; shift 2 ;;
    --only)       ONLY="$2"; shift 2 ;;
    --keep)       KEEP=1 ;;
    *) err "unknown argument: $1"; usage 1 ;;
  esac
done

# --------------------------------------------------------------------------
# Run
# --------------------------------------------------------------------------
mkdir -p "$IT_WORK/log"
log "=== llama-integration-test START ==="
log "ref=$LLAMA_REF backend=$BACKEND model=$MODEL llama=$LLAMA_PORT ollama=$OLLA_PORT only=$ONLY"

case "$ONLY" in
  all|build|serve|ollama|pi) ;;
  *) err "--only must be one of: all|build|serve|ollama|pi"; usage 1 ;;
esac

if [ "$ONLY" = all ] || [ "$ONLY" = build ]; then
  fetch_llama_cpp "$LLAMA_REF"; ensure_model; do_build
fi
if [ "$ONLY" = all ] || [ "$ONLY" = serve ]; then
  ensure_model; do_serve
fi
if [ "$ONLY" = all ] || [ "$ONLY" = ollama ]; then
  do_ollama
fi
if [ "$ONLY" = all ] || [ "$ONLY" = pi ]; then
  do_pi
fi

if [ "$ONLY" = all ]; then
  echo
  log "=== PASS: end-to-end llama.cpp -> Olla -> pi probe succeeded ==="
fi
exit 0
