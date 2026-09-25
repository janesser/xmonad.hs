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

# Run llama-server with default parameters
##  --mlock --no-mmap

# FIXME log file is re-used/overwritten by slave process actually loading the model, on the other hand other params aren't passed
# FIXME router process starts router process (no typo)

# workaround
# Bind localhost-only on a private port so the Olla proxy (systemd unit
# olla.service) can own :8080 publicly. The model is referenced by the short
# symlink ~/.cache/huggingface/hub/ornith.gguf -> this blob, so callers use a
# tidy model name instead of the 150-char HF path.
llama serve \
  --host 127.0.0.1 \
  --port 8081 \
  --model ~/.cache/huggingface/hub/ornith.gguf \
  --log-file $LOG_FILE \
  >/dev/null 2>/dev/null \
  &;disown

echo llama-server \
  --host 127.0.0.1 \
  --port 8081 \
  --models-max 1 \
  --parallel 1 \
  --no-warmup \
  --no-ui \
  --offline \
&; disown

#  --models-preset ~/.llama-cpp-models-preset.ini \
#  --verbosity 3 \
#  --log-file $LOG_FILE \
#  --sleep-idle-seconds 3600 \

echo "llama-server (re-)started."
