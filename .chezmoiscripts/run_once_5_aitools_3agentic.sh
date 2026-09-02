#!/bin/bash

sudo apt install -y fd-find
sudo apt remove --purge -y fdclone
## https://github.com/earendil-works/pi/issues/3882
ln -sf /usr/bin/fdfind ~/.pi/agent/bin/fd

npm i -g @earendil-works/pi-coding-agent
# npx @robzolkos/lazypi --yes

pi install npm:pi-grill
pi install npm:pi-web-access
pi install npm:pi-simplify
pi install npm:@pi-unipi/ralph
pi install npm:pi-subagents
pi install npm:@hypabolic/crossbar
# pi install npm:pi-animations

sudo snap install ghidra
pi install npm:pi-ghidra

# run /web-tools for websearch, e.g. exa.ai free tier

# https://www.canirun.ai/device/rtx-2060
if mountpoint ~/.cache/huggingface/hub; then
    ## hf cache ls --format json | tail -n -1 | jq 'map(.id) | join(",")'
    uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B-it-mmproj.gguf
    uvx hf download hf://google/gemma-4-E4B-it-qat-q4_0-gguf/gemma-4-E4B_q4_0-it.gguf
fi
