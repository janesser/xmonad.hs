#!/usr/bin/fish

# Kill any existing instances of llama-server
killall llama-server

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
set LOG_FILE $LOG_DIR/$(date -d "today" +"%Y%m%d%H%M").log
# echo logging to $LOG_FILE

# Run llama-server with default parameters
##  --mlock
##  --spec-default \
##  --cpu-moe \

llama-server \
  --no-mmap \
  --models-max 1 \
  --parallel 1 \
  --no-warmup \
  --offline \
  --sleep-idle-seconds 240 \
  --jinja \
  --models-preset ~/.llama-cpp-models-preset.ini \
  --verbosity 1 \
&; disown
## --log-file $LOG_FILE \
## 2>/dev/null >/dev/null \

