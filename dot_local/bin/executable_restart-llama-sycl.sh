#!/bin/bash
# restart-llama-sycl.sh — launch llama.cpp on the Intel GPU (SYCL / oneAPI).
#
# Mirrors restart-llama-cuda.sh for the Intel backend: router mode via the
# unified `llama serve` CLI (one instance serves every SYCL model discovered
# from the bind-mounted HF cache), bound to 127.0.0.1:8082 so Olla can own
# :8082 publicly and proxy the SYCL backend. Logs go to journalctl (no
# --log-file), exactly like the CUDA side.
#
# Reap ONLY our own SYCL backend: a process named llama whose cmdline is bound
# to :8082. This never matches the CUDA backend on :8081 (whose binary is also
# literally named `llama-server`) because the match is port-bound. bash is used
# for reliable /proc/<pid>/cmdline scanning plus a bounded SIGTERM->SIGKILL
# wait. The extra `sycl` arg is bash -c's $0 (its script name); $1 = port,
# $2 = mode. (OneAPI's setvars.sh references variables that are unset under
# `set -u`, so we deliberately do NOT use `set -u`.)
#
# Usage:
#   restart-llama-sycl.sh [live|dry|stop] [port]
#     live  (default) reap any stale :8082 orphan, then (re)start the router
#     dry   report what would be reaped, kill nothing
#     stop  reap + umount the shared HF cache
export HOME=/home/jan
USER="${USER:-$(id -un)}"
LIB="$HOME/.local/share/llama-cpp/lib.sh"
[ -f "$LIB" ] && . "$LIB"

MODE="${1:-live}"
PORT="${2:-8082}"

if [ "$MODE" = dry ]; then
    bash -c '
        port="$1"
        for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
            [ -r "/proc/$pid/cmdline" ] || continue
            cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
            case "$cmd" in *"$port"*)
                echo "restart-llama-sycl: DRY-RUN would reap pid $pid ($(cat /proc/$pid/comm 2>/dev/null)) on :$port" ;;
            esac
        done
    ' "$PORT"
    exit 0
fi

if [ "$MODE" = stop ]; then
    sudo umount ~/.cache/huggingface/hub 2>/dev/null || true
    echo "restart-llama-sycl: stopped."
    exit 0
fi

# --- reap any stale :8082 SYCL backend (port-bound; never a sibling backend) ---
bash -c '
    port="$1"; mode="$2"
    got=0
    for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
        [ -r "/proc/$pid/cmdline" ] || continue
        cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
        case "$cmd" in
            *"$port"*)
                comm=$(cat "/proc/$pid/comm" 2>/dev/null)
                echo "restart-llama-sycl: reaping SYCL backend pid $pid ($comm) on :$port"
                kill "$pid" 2>/dev/null
                for i in $(seq 1 10); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
                kill -0 "$pid" 2>/dev/null && { echo "restart-llama-sycl: SIGKILL unresponsive $pid" >&2; kill -9 "$pid" 2>/dev/null; }
                got=1 ;;
        esac
    done
    [ "$got" = 0 ] && echo "restart-llama-sycl: no SYCL backend on :$port to reap"
    sleep 2
' "$PORT" "$MODE"

# --- ensure the SYCL build exists (one-time bootstrap: ~1.5 GB oneAPI + compile) ---
# lib.sh sets BUILD_SYCL, ONEAPI_SETVARS, and llama_sycl_ready().
if ! llama_sycl_ready; then
    # Under systemd we must NOT kick off the multi-minute download+compile inside
    # the oneshot's 120s window (it would time out and fail-loop on every boot).
    # The build is a one-time manual step. An unbuilt backend under systemd is a
    # clean no-op (ExecStartPre already exited 0); once built, `systemctl restart
    # llama-sycl` picks it up.
    if [[ -d /run/systemd/system ]]; then
        echo "restart-llama-sycl: SYCL build not present — bootstrap manually once (~1.5 GB oneAPI + compile), then `systemctl restart llama-sycl`." >&2
        exit 0
    fi
    echo "restart-llama-sycl: SYCL not ready — bootstrapping oneAPI + build..." >&2
    build_sycl "${LLAMA_REF:-origin/master}" || { echo "restart-llama-sycl: SYCL build failed" >&2; exit 1; }
fi

# --- source oneAPI so the SYCL runtime libs are on LD_LIBRARY_PATH.
# lib.sh already sets ONEAPI_SETVARS to the TOP-LEVEL setvars.sh (sources the
# UMF + all component vars). Do NOT override it with the compiler component's
# vars.sh: that skips UMF setup and the SYCL runtime then enumerates no device.
if [ -f "$ONEAPI_SETVARS" ]; then
    export SETVARS_QUIET=1
    . "$ONEAPI_SETVARS"
fi

# Auto-select the Intel GPU. DG1 is not exposed as a Level-Zero device on this
# box, so do NOT force level_zero:N — leave the selector unset for auto-select.
# (The explicit --device SYCL0 below is what actually pins the GPU; this only
# clears any stale override.)
if [ -n "${ONEAPI_DEVICE_SELECTOR:-}" ]; then export ONEAPI_DEVICE_SELECTOR; else unset ONEAPI_DEVICE_SELECTOR; fi

# --- ensure the default model (LFM2.5-2.6B) is visible in the HF cache so the
# router exposes it. Mirrors the CUDA side's ornith.gguf symlink: a tidy,
# stable name so Olla/pi-agent route "LFM2.5" instead of the ~90-char HF blob
# path. Idempotent — refreshed if the blob hash ever changes. ---
model_link="$HOME/.cache/huggingface/hub/LFM2.5-2.6B.gguf"
repo_dir="$HOME/.cache/huggingface/hub/models--liquidai--LFM2.5-2.6B-GGUF"
if [ ! -e "$model_link" ] && [ -d "$repo_dir" ]; then
    blob=$(find "$repo_dir" -name blobs -maxdepth 2 -type d -exec find {} -maxdepth 1 -type f \
            ! -name '*.downloadInProgress' ! -name '*.uploadInProgress' -printf '%s\t%p\n' \; \
            2>/dev/null | sort -rn | head -1 | cut -f2)
    if [ -n "$blob" ]; then
        ln -sf "$blob" "$model_link"
        echo "restart-llama-sycl: exposed cached $model_link -> $blob"
    fi
fi

LOG_DIR="$HOME/.local/log"
mkdir -p "$LOG_DIR"; chmod 700 "$LOG_DIR"; chown -R "$USER" "$LOG_DIR"

# --- run llama.cpp in router mode ---
# One router-server instance serves every SYCL model discovered from the
# bind-mounted HF cache; llama.cpp loads only ONE into VRAM at a time and
# reloads on selection (Olla discovers the full portfolio from /v1/models).
# --device SYCL0 pins the Intel GPU: fail fast (unit ExecStartPre) rather than
# silently falling back to CPU when no Intel device is present. Verify the
# exact index after the build with:  $BUILD_SYCL/bin/llama serve --list-devices
echo "restart-llama-sycl: (re-)starting router on 127.0.0.1:$PORT (Intel GPU, --device SYCL0)"
"$BUILD_SYCL/bin/llama" serve \
  --host 127.0.0.1 --port "$PORT" \
  --models-max 1 --parallel 1 \
  --device SYCL0 --no-ui \
  &
disown

echo "restart-llama-sycl: llama-server (re-)started."
