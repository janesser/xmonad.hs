#!/usr/bin/fish

# Kill any existing instances of llama-server
killall llama-server

# Run llama-server with default parameters
llama-server \
  --no-mmap \
  --ctx-size 100000 \
  --models-max 1 \
  --spec-default \
&; disown
