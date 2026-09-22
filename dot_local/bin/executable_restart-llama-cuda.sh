#!/usr/bin/fish

# Kill any existing instances of llama-server
if killall llama-server || killall llama
  sleep 10
end

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
