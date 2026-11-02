#!/bin/bash

PORT="${PORT:-8081}"

MODEL_FILE="$HOME/.config/card2/little-brother.model"
MODEL_DEFAULT="InternScience/Sage-A1.5-Q4_K_M-GGUF"

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

if ! mountpoint "$HOME/.cache/huggingface/hub" 2>/dev/null; then
    echo WARNING: HF cache is not a mountpoint — llama will fetch the model on demand if missing." >&2
fi

exec llama serve \
    --host :: \
    --port "$PORT" \
    --parallel 1 \
    -dev CUDA0 \
    --metrics \
    --no-ui \
    --hf-repo "$MODEL" \
    -kvu --cache-idle-slots \
    --temp 1.0 \
    --top-p 0.95 \
    --top-k 20 \
    --min-p 0.0 \
    --presence-penalty 1.5 \
    --repeat-penalty 1.0 \
    --flash-attn on
