#!/bin/bash
# restart-llama-cuda.sh — launch llama.cpp on NVIDIA CUDA (router mode, :8081).
#
# This is the launcher for llama-cuda.service, which is now Type=exec: the
# script reaps any stale :8081 orphan, then `exec`s `llama serve` in the
# FOREGROUND. Because exec replaces this shell, systemd tracks the
# llama-server process itself as the unit's main process, so `systemctl stop`
# (KillMode=control-group, the default) kills the real backend instead of just
# the launcher. THIS is what makes the unit genuinely stoppable.
#
# Reap ONLY our own CUDA backend: a process named llama / llama-server whose
# cmdline is bound to OUR port (:8081). This must never match the little
# brother's card-2 backend on :8082 (llama-little-brother), whose llama.cpp
# binary is also literally named `llama-server` -- a blunt killall would kill
# it too; the match is port-bound. Bounded SIGTERM -> SIGKILL wait.
#
# Usage:
#   restart-llama-cuda.sh [live|dry|stop]
#     live  (default) reap any stale :8081 orphan, then (re)start the router
#     dry   report what would be reaped, kill nothing
#     stop  (delegates the real stop to `systemctl stop`; this only umounts the
#           shared HF cache if called directly)
set -u

PORT=8081
MODE="${1:-live}"
GGML_CUDA_P2P=1

if [ "$MODE" = dry ]; then
    echo "restart-llama-cuda: DRY-RUN would reap stale :$PORT orphans (none killed)"
    exit 0
fi

if [ "$MODE" = stop ]; then
    sudo umount ~/.cache/huggingface/hub 2>/dev/null || true
    echo "restart-llama-cuda: stopped (the systemd unit owns the process now)."
    exit 0
fi

# live: reap any stale :8081 orphan (port-bound; never the little-brother card-2 backend on :8082).
got=0
for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
    case "$cmd" in
        *"$PORT"*)
            comm=$(cat "/proc/$pid/comm" 2>/dev/null)
            echo "restart-llama-cuda: reaping stale :$PORT backend pid $pid ($comm)"
            kill "$pid" 2>/dev/null
            for i in $(seq 1 10); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
            kill -0 "$pid" 2>/dev/null && { echo "restart-llama-cuda: SIGKILL unresponsive $pid" >&2; kill -9 "$pid" 2>/dev/null; }
            got=1 ;;
    esac
done
[ "$got" = 0 ] && echo "restart-llama-cuda: no stale :$PORT backend to reap"
sleep 2

# Pre-mount the shared HF cache if it is not already a mountpoint. Under systemd
# the bind mount is in /etc/fstab, so at boot this branch is skipped and the
# script never needs sudo mount.
if ! mountpoint ~/.cache/huggingface/hub; then
    sudo mount -o bind /media/passeport/huggingface-hub/ ~/.cache/huggingface/hub/
fi

mkdir -p ~/.local/log
chmod 700 ~/.local/log
chown -R "$USER" ~/.local/log

# Exec in the foreground so systemd tracks the server (Type=exec). Router mode:
# one instance serves every llama-cuda model discovered from the bind-mounted
# HF cache (~/.cache/huggingface/hub) by llama.cpp's cache loader -- no
# --models-preset/--models-dir. The exposed portfolio is the cache contents,
# and --models-max 1 keeps a single model in VRAM at a time (reloaded on
# selection). Olla discovers them all via /v1/models (dynamic discovery).
# --device CUDA0 pins the GPU: fail fast (unit ExecStartPre) rather than
# silently falling back to CPU when VRAM is unusable. Output flows to
# journalctl (no --log-file).
exec llama serve \
    --host :: \
    --port "$PORT" \
    --models-max 1 \
    --parallel 1 -cram 0 --cache-idle-slots \
    -sps 0.0 \
    -dev CUDA0 \
    --metrics \
    --no-ui
