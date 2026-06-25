#!/bin/bash

cd ~/projs
git clone https://github.com/ggml-org/llama.cpp
cd llama.cpp

cmake -B build -DGGML_CUDA=ON
cmake --build build --config Release

# TODO make build/bin/llama available on PATH