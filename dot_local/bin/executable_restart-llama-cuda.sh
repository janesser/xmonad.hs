#!/usr/bin/fish

# Reap ONLY our own llama-cuda backend: a process named llama / llama-server
# whose cmdline is bound to OUR port (:8081). This must never match the SYCL
# backend on :8082, whose llama.cpp binary is also literally named
# `llama-server` -- a blunt `killall llama-server` would kill it too. Run in
# bash for reliable /proc/<pid>/cmdline scanning plus a bounded
# SIGTERM -> SIGKILL wait. The extra `cuda` arg is `bash -c`'s $0 (its script
# name); $1 = port, $2 = mode. (The `set -u`-free fish launcher inherits this.)
bash -c '
  port="$1"; mode="$2"
  got=0
  for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr "\0" " " < "/proc/$pid/cmdline" 2>/dev/null)
    case "$cmd" in
      *"$port"*)
        comm=$(cat "/proc/$pid/comm" 2>/dev/null)
        if [ "$mode" = dry ]; then
          echo "restart-llama-cuda: DRY-RUN would reap pid $pid ($comm) on :$port"
        else
          echo "restart-llama-cuda: reaping llama-cuda backend pid $pid ($comm) on :$port"
          kill "$pid" 2>/dev/null
          for i in $(seq 1 10); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
          kill -0 "$pid" 2>/dev/null && { echo "restart-llama-cuda: SIGKILL unresponsive $pid" >&2; kill -9 "$pid" 2>/dev/null; }
        fi
        got=1
        ;;
    esac
  done
  [ "$got" = 0 ] && echo "restart-llama-cuda: no llama-cuda backend on :$port to reap"
  [ "$mode" != dry ] && sleep 2
' cuda 8081 live

if [ "$argv[1]" = "stop" ]
  sudo umount ~/.cache/huggingface/hub
  echo llama-server stopped, exiting.
  exit 0
end

# pre-mount cache
if ! mountpoint ~/.cache/huggingface/hub
  sudo mount -o bind /media/passeport/huggingface-hub/ ~/.cache/huggingface/hub/
end

set LOG_DIR ~/.local/log
mkdir -p $LOG_DIR
chmod 700 $LOG_DIR
chown -R $USER $LOG_DIR
#set LOG_FILE $LOG_DIR/$(date -d "today" +"%Y%m%d%H%M").log
set LOG_FILE $LOG_DIR/llama-server.log
# echo logging to $LOG_FILE

# Run llama-server in router mode.
##  --mlock --no-mmap

# Bind on all interfaces (IPv6 wildcard) on a private port. Models are served
# in router mode: discovered automatically from the bind-mounted HF cache
# (~/.cache/huggingface/hub) by llama.cpp's cache loader -- no --models-preset,
# no --models-dir, no -hf needed. The exposed portfolio is therefore the cache
# contents, and --models-max 1 keeps a single model in VRAM at a time.
# Output flows to journalctl (no --log-file, no >/dev/null suppression).
llama serve \
  --host :: \
  --port 8081 \
  --models-max 1 \
  --parallel 1 \
  --device CUDA0 \
  --no-ui \
  &;disown

# One router-server instance serves every llama-cuda model in the preset; llama.cpp
# loads only ONE into VRAM at a time and reloads on selection. Olla discovers all of
# them from /v1/models (dynamic discovery), so it now exposes the full portfolio.
# Ids are preset repo ids (e.g. deepreinforce-ai/Ornith-1.5-35B-A3B-GGUF:Q4_K_M),
# not the old ~/.cache/huggingface/hub/ornith.gguf path -> update any pinned client.

echo "llama-server (re-)started."
