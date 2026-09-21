#!/bin/bash
# restart-llama-sycl.sh — launch llama.cpp on the Intel GPU (SYCL / oneAPI).
#
# Mirrors restart-llama-server.sh for the Intel backend: binds localhost-only on
# port 8082 so Olla can own :8082 publicly and proxy gemma-4-E4B.
#
# This launcher EXECs the llama-server as its own process (no fork/disown), so
# systemd (Type=simple) tracks it directly and manages restart/kill cleanly.
# It is also safe to run by hand: it blocks in the foreground until you Ctrl-C.
#
# OneAPI's setvars.sh references variables that are unset under `set -u`, so we
# deliberately do NOT use `set -u`.
export HOME=/home/jan
USER="${USER:-$(id -un)}"

# Lazily bootstrap oneAPI + the SYCL build on first use (no-sudo, user-run).
LIB="$HOME/.local/share/llama-cpp/lib.sh"
[ -f "$LIB" ] && . "$LIB"
if ! llama_sycl_ready; then
    echo "restart-llama-sycl: SYCL not ready — bootstrapping oneAPI + build..." >&2
    build_sycl origin/master || { echo "restart-llama-sycl: SYCL build failed" >&2; exit 1; }
fi

# Source oneAPI so the SYCL runtime libs are on LD_LIBRARY_PATH. lib.sh already
# sets ONEAPI_SETVARS to the TOP-LEVEL setvars.sh (which sources the UMF and all
# component vars). Do NOT override it with the compiler component's vars.sh: that
# one skips UMF setup and the SYCL runtime then enumerates no device.
if [ -f "$ONEAPI_SETVARS" ]; then
    export SETVARS_QUIET=1
    . "$ONEAPI_SETVARS"
fi

# Auto-select the Intel GPU. DG1 is not exposed as a Level-Zero device on this
# box, so do NOT force level_zero:N — leave the selector unset for auto-select.
unset ONEAPI_DEVICE_SELECTOR 2>/dev/null || true

PORT="${1:-8082}"
# gemma-4-E4B (Q4_0) fits the Iris Xe (~3.8 GB) shared-memory budget. It is the
# smaller of the two cached gemma models (the 26B-A4B needs ~14 GB).
MODEL="${2:-"$HOME/.cache/huggingface/hub/models--google--gemma-4-E4B-it-qat-q4_0-gguf/snapshots/4b4a2c1d584be7264f87aac328a1bc739ce81b6c/gemma-4-E4B_q4_0-it.gguf"}"
if [ ! -e "$MODEL" ]; then
    echo "restart-llama-sycl: model not found at $MODEL" >&2
    exit 1
fi

LOG_DIR="$HOME/.local/log"
mkdir -p "$LOG_DIR"; chmod 700 "$LOG_DIR"; chown -R "$USER" "$LOG_DIR"
LOG_FILE="$LOG_DIR/llama-sycl.log"

echo "restart-llama-sycl: exec-ing llama-server (Intel GPU) on 127.0.0.1:$PORT with $MODEL"
# exec (not fork) so systemd Type=simple tracks this process directly.
exec "$BUILD_SYCL/bin/llama-server" \
  --host 127.0.0.1 --port "$PORT" --model "$MODEL" \
  --log-file "$LOG_FILE"
