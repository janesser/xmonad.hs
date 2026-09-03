#!/bin/bash

LLAMA_MODELS_PRESET=~/.llama-cpp-models-preset.ini

hx ~/.llama-cpp-models-preset.ini

if [[ -n `chezmoi diff $LLAMA_MODELS_PRESET` ]]; then
  restart-llama-server.sh
fi

chezmoi re-add ~/.llama-cpp-models-preset.ini

tail -f ~/.local/log/llama-server.log
