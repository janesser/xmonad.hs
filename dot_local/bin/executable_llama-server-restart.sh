#!/usr/bin/fish

# pre-mount cache
if mountpoint ~/.cache/huggingface/hub
  echo external model cache is mounted.
else
  sudo mount -o bind /media/jan/passeport/huggingface-hub/ ~/.cache/huggingface/hub/
end 

# Kill any existing instances of llama-server
llama-server-stop.sh

# Run llama-server with default parameters
llama-server \
  --no-mmap \
  --ctx-size 100000 \
  --models-max 1 \
  --spec-default \
  --offline \
  --verbosity 2 \
&; disown

# tail sync
pi-update-llama-models.sh # models auto-generated
cz re-add ~/.pi/agent/settings.json
