#!/bin/bash

lsmod | grep nvidia
if [[ $? -ne 0 ]]; then
    echo "$(basename $0): No nvidia module loaded in kernel, skipping..."
    exit 0
fi

cd ~/projs
git clone https://github.com/ggml-org/llama.cpp
cd llama.cpp

cmake -B build -DGGML_CUDA=ON
cmake --build build --config Release

# use llama.cpp router mode
## see .local/bin/llama-server-restart.sh

# make build/bin/llama available on PATH
## see .local/bin/llama and llama-server symlinks

# CUDA init error on suspend/resume
## FIXED https://github.com/ggml-org/llama.cpp/issues/7218