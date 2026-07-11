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

# Run llama-server with default parameters
llama-server \
  --fit on \
  --mlock \
  --no-mmap \
  --ctx-size 100000 \
  --models-max 1 \
  --spec-default \
  --cpu-moe \
  --cache-type-k q4_0 \
  --cache-type-v q4_0 \
  --offline \
  --verbosity 2 \
  --image-min-tokens 1024 \
  --sleep-idle-seconds 240 \
&; disown
