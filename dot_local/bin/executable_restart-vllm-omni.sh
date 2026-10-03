#!/usr/bin/env bash
# restart-vllm-omni.sh — launch/stop vLLM-Omni (omni models) on the V100 (:8091).
#
# vLLM-Omni shares the single V100 (32 GB) with the llama.cpp CUDA backend
# (:8081, ornith). Only ONE can hold the V100 at a time, so free VRAM from the
# other backend BEFORE starting — see switch-ai-backend.sh.
#
# start [MODEL]: reap any stale :8091, confirm the NVIDIA driver is up, activate
#   the venv (which exports LD_LIBRARY_PATH so torchcodec finds torch's bundled
#   CUDA-13 runtime under nvidia/cu13/lib), then fork `vllm serve … --omni
#   --port 8091` and wait until it answers /v1/models.
# stop: reap the :8091 backend (free VRAM).
#
# No sudo needed: the HF cache bind mount is already in fstab and the venv has
# no pip (the `vllm` binary resolves via the activated venv).
set -uo pipefail

VENV=/media/sailor/ai-server
PORT=8091
MODEL="Qwen/Qwen2.5-Omni-7B"
[ "${1:-}" != "" ] && MODEL="$1"
HF_HOME="${HF_HOME:-$HOME/.cache/huggingface}"
LOG_DIR="$HOME/.local/log"

# Port-bound reap: kill only the api_server process whose cmdline carries $PORT.
reap() {
  local port="$1" pid cmd
  for pid in $(pgrep -f "api_server" 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null)
    case "$cmd" in *"$port"*)
      echo "restart-vllm-omni: reaping vLLM-Omni pid $pid on :$port"
      kill "$pid" 2>/dev/null
      for _ in $(seq 1 15); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
      kill -0 "$pid" 2>/dev/null && { echo "restart-vllm-omni: SIGKILL $pid" >&2; kill -9 "$pid" 2>/dev/null; }
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

case "${1:-start}" in
  stop)
    reap "$PORT"
    echo "restart-vllm-omni: vLLM-Omni stopped on :$PORT."
    exit 0
    ;;
  start | "")
    ;;
  *)
    echo "usage: $0 [start MODEL | stop]" >&2; exit 2 ;;
esac

# NVIDIA driver must be up or vLLM loads weights into system RAM and OOMs.
if ! nvidia-smi >/dev/null 2>&1; then
  echo "restart-vllm-omni: nvidia-smi failed — NVIDIA driver not ready. Aborting." >&2
  exit 1
fi

mkdir -p "$LOG_DIR"; chmod 700 "$LOG_DIR"; chown -R "$USER" "$LOG_DIR" 2>/dev/null || true
reap "$PORT"

cd "$VENV" || exit 1
# activate sets PATH (vllm resolves to the venv) AND LD_LIBRARY_PATH (torchcodec
# cu132 runtime) — both required for the import to succeed on the V100.
# shellcheck disable=SC1091
source "$VENV/.venv/bin/activate"
export HF_HOME

echo "restart-vllm-omni: launching vllm serve $MODEL --omni --port $PORT (max-model-len ${MAX_LEN:-12288})"
nohup vllm serve "$MODEL" --omni \
  --port "$PORT" \
  --host 127.0.0.1 \
  --max-model-len "${MAX_LEN:-12288}" \
  --log-file "$LOG_DIR/vllm-omni.log" \
  >/dev/null 2>&1 &
disown

if wait_ready "$PORT"; then
  echo "restart-vllm-omni: vLLM-Omni healthy on :$PORT."
else
  echo "restart-vllm-omni: vLLM-Omni did not become healthy on :$PORT." >&2
  exit 1
fi
