#!/bin/bash

lsmod | grep nvidia
if [[ $? -ne 0 ]]; then
    echo "$(basename $0): No nvidia module loaded in kernel, skipping..."
    exit 0
fi

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

pi install npm:pi-llama-cpp # auto-discover models available
pi install npm:@juicesharp/rpiv-pi 
# run /rpiv-setup within pi to install required extensions
# run /web-tools for websearch, e.g. exa.ai free tier

if mountpoint ~/.cache/huggingface/hub; then
    uvx hf download hf://jica98/qwen3.5-4B-super-coder/qwen3.5-4B-super-coder.BF16-mmproj.gguf
    uvx hf download hf://jica98/qwen3.5-4B-super-coder/qwen3.5-4B-super-coder.Q4_0.gguf
    uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B-it-mmproj.gguf
    uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B_q4_0-it.gguf
fi
