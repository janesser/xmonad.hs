#!/bin/bash
# restart-llama-cuda.sh — launch llama.cpp on NVIDIA CUDA (single model, :8081).
# Mirrors the card-2 (8082) launcher: one model, parameters embedded as direct
# CLI args. Once card2 is healthy, these flags are the copy for card1.
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

# Model: one line in ~/.config/card1/llama-cuda.model (HF repo id or local GGUF
# path — the "varying second model"). Change the model = edit that file +
# `systemctl restart llama-cuda`.
MODEL_FILE="$HOME/.config/card1/llama-cuda.model"
MODEL_DEFAULT="InternScience/Sage-A1.5-Q4_K_M-GGUF"

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

# One-line model file (the "varying second model"); fall back to the default.
MODEL="$MODEL_DEFAULT"
if [ -f "$MODEL_FILE" ]; then
    read -r MODEL < "$MODEL_FILE" || true
    [ -n "$MODEL" ] || MODEL="$MODEL_DEFAULT"
fi
echo "restart-llama-cuda: serving $MODEL on :$PORT (card 1)"

# Pre-mount the shared HF cache if it is not already a mountpoint. Under systemd
# the bind mount is in /etc/fstab, so at boot this branch is skipped and the
# script never needs sudo mount.
if ! mountpoint ~/.cache/huggingface/hub; then
    sudo mount -o bind /media/passeport/huggingface-hub/ ~/.cache/huggingface/hub/
fi

mkdir -p ~/.local/log
chmod 700 ~/.local/log
chown -R "$USER" ~/.local/log

# Exec in the foreground so systemd tracks the server (Type=exec). Single model:
# the model is the one pinned by the unit's CUDA_VISIBLE_DEVICES (card 1),
# parameters are embedded as direct CLI args (see the card-2 launcher for the
# same set). -cram 0 disables the prompt cache (decision-model default);
# --cache-idle-slots is inert with cram 0 but kept default-on.
# --device CUDA0 pins the GPU: fail fast (unit ExecStartPre) rather than
# silently falling back to CPU when VRAM is unusable. Output flows to
# journalctl (no --log-file).
exec llama serve \
    --host :: \
    --port "$PORT" \
    --parallel 1 \
    --cache-ram 0 \
    --cache-idle-slots \
    -sps 0.0 \
    --flash-attn on \
    --ctx-size 262144 \
    --n-gpu-layers 99 \
    --hf-repo "$MODEL" \
    --temp 1.0 \
    --top-p 0.95 \
    --top-k 20 \
    --min-p 0.0 \
    --presence-penalty 1.5 \
    --repeat-penalty 1.0 \
    -dev CUDA0 \
    --metrics \
    --no-ui
