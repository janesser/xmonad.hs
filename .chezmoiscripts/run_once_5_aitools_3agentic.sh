#!/bin/bash

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

nvm use --lts
sudo apt install -y fd-find
## https://github.com/earendil-works/pi/issues/3882
ln -sf /usr/bin/fdfind ~/.pi/agent/bin/fd
npm i -g @earendil-works/pi-coding-agent

# use pi with local llama.cpp
## https://github.com/opensecurity/code-offline/tree/main
## see 'pi-update-llama-models.sh'

pi install npm:pi-web-access

uvx hf download hf://nvidia/NVIDIA-Nemotron-3-Nano-4B-GGUF/NVIDIA-Nemotron3-Nano-4B-Q4_K_M.gguf
uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B-it-mmproj.gguf
uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B_q4_0-it.gguf
