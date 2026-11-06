#!/bin/bash

PORT="${PORT:-8081}"
MODEL="InternScience/Sage-A1.5-Q4_K_M-GGUF"
CTX=0

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
            kill -0 "$pid" 2>/dev/null && { echo "SIGKILL unresponsive $pid" >&2; kill -9 "$pid" 2>/dev/null; }
            got=1 ;;
    esac
done
[ "$got" = 0 ] && echo "no stale :$PORT backend to reap"
sleep 2

if ! mountpoint "$HOME/.cache/huggingface/hub" 2>/dev/null; then
    echo "WARNING: HF cache is not a mountpoint — llama will fetch the model on demand if missing." >&2
fi

exec llama serve \
    --host :: \
    --port "$PORT" \
    -dev CUDA0 \
    --metrics \
    --no-ui \
    -hf "$MODEL" --ctx-size "$CTX" \
    --temp 1.0 \
    --top-p 0.95 \
    --top-k 20 \
    --min-p 0.0 \
    --presence-penalty 1.5 \
    --repeat-penalty 1.0 \
    --parallel 1 \
    -kvu -cram 12288 --cache-idle-slots --cache-prompt \
    --flash-attn on \
    --ctx-checkpoints 256 \
    --checkpoint-min-step 1024 \
    -lv 4
