#!/usr/bin/fish

# Reap ONLY our own llama-cuda backend: a process named llama / llama-server
# whose cmdline is bound to OUR port (:8081). This must never match the SYCL
# backend on :8082, whose llama.cpp binary is also literally named
# `llama-server` -- a blunt `killall llama-server` would kill it too. Run in
# bash for reliable /proc/<pid>/cmdline scanning plus a bounded
# SIGTERM -> SIGKILL wait. The extra `cuda` arg is `bash -c`'s $0 (its script
# name); $1 = port, $2 = mode. (The `set -u`-free fish launcher inherits this.)

# --- configurable knobs (override via environment) --------------------------
#   LLAMA_HOST       bind address of the backend       (default 127.0.0.1)
#   LLAMA_PORT       TCP port of the backend           (default 8081)
#   LLAMA_MODEL_NAME model id: a local path, a .gguf
#                    symlink under the HF cache (default), or an HF repo
#                    "<user>/<model>[:quant]" downloaded on first use.
#                    (this machine: ornith-1.5 9B)
set -l LLAMA_HOST   $env.LLAMA_HOST;   if test -z "$LLAMA_HOST";   set LLAMA_HOST 127.0.0.1; end
set -l LLAMA_PORT   $env.LLAMA_PORT;   if test -z "$LLAMA_PORT";   set LLAMA_PORT 8081; end
set -l LLAMA_MODEL_NAME $env.LLAMA_MODEL_NAME; if test -z "$LLAMA_MODEL_NAME"; set LLAMA_MODEL_NAME "ornith-1.5-9B.gguf"; end
set -l HF_HUB "$HOME/.cache/huggingface/hub"

# Resolve LLAMA_MODEL_NAME to something `llama serve` can open: an existing
# local path, a .gguf symlink under the HF cache, or an HF repo
# "<user>/<model>[:quant]" served from its cached blob (or downloaded via
# --hf-repo if it is not cached yet).
#   -> "<local path or blob>"   success
#   -> "HF_REPO:<repo>"         not cached; serve via --hf-repo
#   -> "MISSING:<id>" (exit 1)  nothing to serve
set -l MODEL_PATH (bash -c '
  model="$1"; hub="$2"
  [ -e "$model" ] && { printf "%s\n" "$model"; exit 0; }
  [ -e "$hub/$model" ] && { printf "%s\n" "$hub/$model"; exit 0; }
  case "$model" in
    */*)
      repo="${model%:*}"
      cdir="$hub/models--$(printf "%s" "$repo" | sed "s#/#--#g")"
      blob=$(find "$cdir/blobs" -maxdepth 1 -type f ! -name "*.downloadInProgress" ! -name "*.uploadInProgress" -printf "%s\t%p\n" 2>/dev/null | sort -rn | head -1 | cut -f2)
      if [ -n "$blob" ]; then printf "%s\n" "$blob"; exit 0; fi
      printf "HF_REPO:%s\n" "$model"; exit 0
      ;;
  esac
  printf "MISSING:%s\n" "$model"; exit 1
' "_LLAMA" "$LLAMA_MODEL_NAME" "$HF_HUB")

# Turn the resolved path into the right serve args.
set -l MODEL_ARGS --model "$MODEL_PATH"
if string startswith "HF_REPO:" "$MODEL_PATH"
    set MODEL_ARGS --hf-repo (string sub -s 7 "$MODEL_PATH")
else if string startswith "MISSING:" "$MODEL_PATH"
    echo "llama-server-cuda: model not found: $MODEL_PATH" >&2
    exit 1
end
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
' cuda $LLAMA_PORT live

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
# olla.service) can own the public port. HOST/PORT are configurable via env
# (see above); the model is configurable via LLAMA_MODEL_NAME. The model is
# referenced by the short symlink ~/.cache/huggingface/hub/ornith-1.5-9B.gguf
# -> the blob, so callers use a tidy model id instead of the 150-char HF path.
llama serve \
  --host "$LLAMA_HOST" \
  --port "$LLAMA_PORT" \
  $MODEL_ARGS \
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
