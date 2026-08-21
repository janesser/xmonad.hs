#!/bin/bash

LLAMA_MODELS_PRESET=~/.llama-cpp-models-preset.ini

chezmoi edit -a  ~/.llama-cpp-models-preset.ini

if [[ -n `chezmoi diff $LLAMA_MODELS_PRESET` ]]; then
  restart-llama-server.sh
fi

tail -f ~/.local/log/llama-server.log
