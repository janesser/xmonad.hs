#!/usr/bin/fish

# Kill any existing instances of llama-server
killall llama-server
killall llama

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
llama serve \
  --host :: \
  --model ~/.cache/huggingface/hub/models--ornith-ai--Ornith-1.5-35B-A3B-GGUF/snapshots/12393612fd4f730ff5aadc23e9b8f9648aa49ceb/Ornith-1.5-35B-Q4_K_M.gguf \
  --log-file $LOG_FILE \
  &;disown

echo llama-server \
  --host :: \
  --models-max 2 \
  --parallel 1 \
  --no-warmup \
  --no-ui \
  --offline \
  --models-preset ~/.llama-cpp-models-preset.ini \
  --verbosity 3 \
  --log-file $LOG_FILE \
  2>/dev/null >/dev/null \
&; disown

#  --sleep-idle-seconds 3600 \

echo "llama-server (re-)started."
