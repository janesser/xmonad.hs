#!/bin/bash
# start-little-brother.sh — launch the little brother's llama.cpp text backend
# on card 2 (PCIe V100), [::]:8082.
#
# This is the launcher for llama-little-brother.service, which is Type=exec:
# the script reaps any stale :8082 orphan, then `exec`s `llama serve` in the
# FOREGROUND. Because exec replaces this shell, systemd tracks the
# llama-server process itself as the unit's main process, so `systemctl stop`
# (KillMode=control-group, the default) kills the real backend instead of just
# the launcher. THIS is what makes the unit genuinely stoppable — which
# card2-swap.sh depends on for the image batches.
#
# Reap ONLY our own card-2 backend: a process named llama / llama-server whose
# cmdline is bound to OUR port (:8082). This must never match the 27B router on
# :8081, whose llama.cpp binary is also literally named `llama` /
# `llama-server` -- a blunt killall would kill it too; the match is port-bound.
# Bounded SIGTERM -> SIGKILL wait.
#
# GPU pinning: the unit sets Environment=CUDA_VISIBLE_DEVICES=<card-2 UUID>
# (single source of truth), so `-dev CUDA0` below is index 0 in the remapped
# space — always card 2, never the 27B card.
#
# Model: one line in ~/.config/card2/little-brother.model (HF repo id or local
# GGUF path — the "varying second model"). Change the model = edit that file +
# `systemctl restart llama-little-brother`.
set -u

PORT=8082
MODEL_FILE="$HOME/.config/card2/little-brother.model"
MODEL_DEFAULT="InternScience/Sage-A1.5-Q4_K_M-GGUF"

# live: reap any stale :8082 orphan (port-bound; never the :8081 router).
got=0
for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
    case "$cmd" in
        *"$PORT"*)
            comm=$(cat "/proc/$pid/comm" 2>/dev/null)
            echo "start-little-brother: reaping stale :$PORT backend pid $pid ($comm)"
            kill "$pid" 2>/dev/null
            for i in $(seq 1 10); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
            kill -0 "$pid" 2>/dev/null && { echo "start-little-brother: SIGKILL unresponsive $pid" >&2; kill -9 "$pid" 2>/dev/null; }
            got=1 ;;
    esac
done
[ "$got" = 0 ] && echo "start-little-brother: no stale :$PORT backend to reap"
sleep 2

# Warn-and-continue if the HF cache bind mount is down (it is an fstab mount,
# up at boot, so this branch is normally skipped). NEVER `sudo mount` — mount
# is not in the scoped sudoers drop-in and would block on a password; a missing
# mount just means llama fetches the model on demand.
if ! mountpoint "$HOME/.cache/huggingface/hub" 2>/dev/null; then
    echo "start-little-brother: WARNING: HF cache is not a mountpoint — llama will fetch the model on demand if missing." >&2
fi

# One-line model file (the "varying second model"); fall back to the default.
MODEL="$MODEL_DEFAULT"
if [ -f "$MODEL_FILE" ]; then
    read -r MODEL < "$MODEL_FILE" || true
    [ -n "$MODEL" ] || MODEL="$MODEL_DEFAULT"
fi
echo "start-little-brother: serving $MODEL on :$PORT (card 2, UUID-pinned by the unit)"

# Exec in the foreground so systemd tracks the server (Type=exec). Output flows
# to journalctl (no --log-file). -dev CUDA0 = the UUID-pinned card (see unit).
# Sage-A1.5 Q4_K_M (21.7 GB, 35B MoE / 3B active) with -c 16384 --parallel 2
# and q4_1 KV fits the 32 GB card exclusive (~24 GB) and alongside an sd.cpp
# image batch via card2-swap.sh --keep-text (~30 GB) with headroom.
exec llama serve \
    --host :: \
    --port "$PORT" \
    --parallel 1 \
    -dev CUDA0 \
    --metrics -lv 4 \
    --no-ui

#    --hf-repo "$MODEL" \
