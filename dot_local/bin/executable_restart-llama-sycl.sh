#!/bin/bash
# restart-llama-sycl.sh — launch llama.cpp on the Intel GPU (SYCL / oneAPI).
#
# Mirrors restart-llama-cuda.sh for the Intel backend: a SINGLE-model
# `llama-server` (NOT router mode) pinned to one GGUF via --hf-repo, bound to
# [::]:8082 so Olla can own :8082 publicly and proxy the SYCL backend. Logs go
# to journalctl (no --log-file), exactly like the CUDA side.
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
    # PORT is read from the environment (exported below), NOT from a positional
    # arg: this environment drops the FIRST positional arg of a nested
    # `bash -c 'script' ARG1 ARG2` call (ARG1 is lost), which used to make the
    # reaper match on an empty port string. Exported vars survive that intact.
    export PORT
    bash -c '
        for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
            [ -r "/proc/$pid/cmdline" ] || continue
            cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
            case "$cmd" in *"$PORT"*)
                echo "restart-llama-sycl: DRY-RUN would reap pid $pid ($(cat /proc/$pid/comm 2>/dev/null)) on :$PORT" ;;
            esac
        done
    '
    exit 0
fi

if [ "$MODE" = stop ]; then
    sudo umount ~/.cache/huggingface/hub 2>/dev/null || true
    echo "restart-llama-sycl: stopped."
    exit 0
fi

# --- reap any stale :8082 SYCL backend (port-bound; never a sibling backend) ---
# PORT/MODE are exported and read from the environment inside the subshell
# instead of passed as positional args — see the dry-run block for why
# (this environment drops the first positional arg of a nested `bash -c`).
export PORT MODE
bash -c '
    got=0
    for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
        [ -r "/proc/$pid/cmdline" ] || continue
        cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
        case "$cmd" in
            *"$PORT"*)
                comm=$(cat "/proc/$pid/comm" 2>/dev/null)
                echo "restart-llama-sycl: reaping SYCL backend pid $pid ($comm) on :$PORT"
                kill "$pid" 2>/dev/null
                for i in $(seq 1 10); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
                kill -0 "$pid" 2>/dev/null && { echo "restart-llama-sycl: SIGKILL unresponsive $pid" >&2; kill -9 "$pid" 2>/dev/null; }
                got=1 ;;
        esac
    done
    [ "$got" = 0 ] && echo "restart-llama-sycl: no SYCL backend on :$PORT to reap"
    sleep 2
'

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

# --- pinned model (single-server, non-router mode) ---
# The SYCL backend serves exactly ONE model, selected with --hf-repo below;
# there is nothing to symlink into the HF cache (no discover-all router), so
# this script is self-contained. The blob already lives in the bind-mounted HF
# cache (models--unsloth--Qwen3.5-0.8B-GGUF/.../Qwen3.5-0.8B-Q4_K_M.gguf) and --offline
# forces cache-only resolution, so boot never stalls on / is redirected to the
# network. ---

LOG_DIR="$HOME/.local/log"
mkdir -p "$LOG_DIR"; chmod 700 "$LOG_DIR"; chown -R "$USER" "$LOG_DIR"

# Repetition penalty settings
# 1.0 is the default - keep it low to prevent loops
export LLAMA_ARG_REPEAT_PENALTY=1.0

# Enable frequency-based penalty if you want it
export LLAMA_ARG_FREQUENCY_PENALTY=1.5

MODEL="LiquidAI/LFM2.5-2.6B-GGUF:Q4_K_M"

# --- run a single pinned model (NOT router mode) ---
# One llama-server instance serves exactly one GGUF, selected with --hf-repo.
# Olla discovers that single model from /v1/models. --device SYCL0 pins the
# Intel GPU: fail fast (unit ExecStartPre) rather than silently falling back to
# CPU when no Intel device is present. Verify the exact index after the build
# with:  $BUILD_SYCL/bin/llama serve --list-devices
echo "restart-llama-sycl: (re-)starting single-model server on [::]:$PORT (Intel GPU, --device SYCL0) serving unsloth/Qwen3.5-0.8B-GGUF:Q4_K_M"
"$BUILD_SYCL/bin/llama-server" \
  --host :: --port "$PORT" \
  --hf-repo $MODEL \
  --offline \
  --device SYCL0 --parallel 1 --no-ui \
  &
disown

echo "restart-llama-sycl: llama-server (re-)started."
