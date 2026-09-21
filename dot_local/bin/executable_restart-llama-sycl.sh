#!/bin/bash
# restart-llama-sycl.sh — launch llama.cpp on the Intel GPU (SYCL / oneAPI).
#
# Mirrors restart-llama-server.sh for the Intel backend: binds localhost-only on
# port 8082 so Olla can own :8082 publicly and proxy the SYCL backend model.
#
# The model is passed via llama.cpp's --hf-repo (`-hf`) form
# `<user>/<model>[:quant]`, so it downloads on first use and stays in the local
# HF cache. A plain local path also works; override with argv[2].
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
# Default to the dense 2.6B LFM2.5 (Q4_K_M, ~1.8 GB weights), which fits the
# Iris Xe shared-memory budget with room to spare for the KV cache — unlike the
# 8B gemma-4-E4B (~3.8 GB) or the 26B-A4B (~14 GB). Override with argv[2]; the
# value may be an HF repo (`<user>/<model>[:quant]`) or a local .gguf path.
MODEL="${2:-LiquidAI/LFM2.5-2.6B-GGUF:Q4_K_M}"   # NOTE: no inner quotes — bash keeps them literal inside ${...:-...}

# HF repos look like `<user>/<model>[:quant]` — no leading slash/dot. Anything
# else is a local path that must exist on disk.
if [[ "$MODEL" == http://* || "$MODEL" == https://* \
   || "$MODEL" == [^/]*/*[^/]* && "$MODEL" != /* && "$MODEL" != .* ]]; then
    HF_MODEL=1
    repo="${MODEL%%:*}"                                  # strip the :quant
    # HF names the cache dir by replacing each '/' with '--' (double dash).
    cache_dir="$HOME/.cache/huggingface/hub/models--$(printf '%s' "$repo" | sed 's#/#--#g')"
    # Serve from the cached blob via --model (fast + offline). The --hf-repo
    # runtime path is unreliable here because the HF cache only holds the blob
    # + ref, not the repo file listing, so llama.cpp's repo lookup fails.
    if [ -d "$cache_dir/blobs" ]; then
        # HF blobs are raw sha256 files with no extension; pick the largest
        # non-in-progress one (a *.{download,upload}InProgress is a partial).
        blob=$(find "$cache_dir/blobs" -maxdepth 1 -type f \
                ! -name '*.downloadInProgress' ! -name '*.uploadInProgress' \
                -printf '%s\t%p\n' 2>/dev/null | sort -rn | head -1 | cut -f2)
    fi
    if [ -n "$blob" ]; then
        # Mirror the CUDA backend's ornith.gguf symlink: expose the model under a
        # tidy, stable name in the HF cache so Olla/pi-agent route "LFM2.5" instead
        # of the ~90-char HF blob path. Refresh it if the blob hash ever changes.
        model_link="$HOME/.cache/huggingface/hub/LFM2.5-2.6B.gguf"
        ln -sf "$blob" "$model_link"
        echo "restart-llama-sycl: serving cached $model_link -> $blob"
        MODEL_ARGS=(--model "$model_link")
    else
        echo "restart-llama-sycl: cached blob not found for $repo — downloading via --hf-repo" >&2
        MODEL_ARGS=(--hf-repo "$MODEL")
    fi
else
    HF_MODEL=0
    if [ ! -e "$MODEL" ]; then
        echo "restart-llama-sycl: model not found at $MODEL" >&2
        exit 1
    fi
    MODEL_ARGS=(--model "$MODEL")
fi

LOG_DIR="$HOME/.local/log"
mkdir -p "$LOG_DIR"; chmod 700 "$LOG_DIR"; chown -R "$USER" "$LOG_DIR"
LOG_FILE="$LOG_DIR/llama-sycl.log"

echo "restart-llama-sycl: exec-ing llama-server (Intel GPU) on 127.0.0.1:$PORT with $MODEL"
# exec (not fork) so systemd Type=simple tracks this process directly.
exec "$BUILD_SYCL/bin/llama-server" \
  --host 127.0.0.1 --port "$PORT" "${MODEL_ARGS[@]}" \
  --log-file "$LOG_FILE"
