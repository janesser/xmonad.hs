#!/bin/bash

sudo "$CHEZMOI_SOURCE_DIR/uninstaller/ollama_uninstall.sh"

lsmod | grep nvidia
if [[ $? -ne 0 ]]; then
    echo "$(basename $0): No nvidia module loaded in kernel, skipping..."
    exit 0
fi

uv tool install gpustat

cd ~/projs
git clone https://github.com/ggml-org/llama.cpp
cd llama.cpp
LATEST_RELEASE=`git tag --sort=-committerdate|head -1`
git checkout $LATEST_RELEASE

sudo apt install -y ccache
cmake -B build -DGGML_CUDA=ON
cmake --build build --config Release -j $(grep processor /proc/cpuinfo | wc -l)

restart-llama-server.sh

# use llama.cpp router mode
## see .local/bin/llama-server-restart.sh

# make build/bin/llama available on PATH
## see .local/bin/llama and llama-server symlinks

# CUDA init error on suspend/resume
## FIXED https://github.com/ggml-org/llama.cpp/issues/7218
