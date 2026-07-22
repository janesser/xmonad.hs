#!/bin/bash

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

nvm use --lts
sudo apt install -y fd-find
sudo apt remove --purge -y fdclone
## https://github.com/earendil-works/pi/issues/3882
ln -sf /usr/bin/fdfind ~/.pi/agent/bin/fd

#npm i -g @earendil-works/pi-coding-agent
npx @robzolkos/lazypi --yes

pi install npm:@hypabolic/crossbar

pi uninstall npm:pi-claude-cli
pi list |grep -o 'npm:@juicesharp/rpiv-.*'| xargs -L1 pi uninstall

# run /web-tools for websearch, e.g. exa.ai free tier

if mountpoint ~/.cache/huggingface/hub; then
    uvx hf download hf://jica98/qwen3.5-4B-super-coder/qwen3.5-4B-super-coder.BF16-mmproj.gguf
    uvx hf download hf://jica98/qwen3.5-4B-super-coder/qwen3.5-4B-super-coder.Q4_0.gguf
    uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B-it-mmproj.gguf
    uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B_q4_0-it.gguf
fi
