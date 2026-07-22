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
git fetch
LATEST_RELEASE=`git tag --sort=-committerdate|head -1`
git checkout $LATEST_RELEASE

sudo apt install -y ccache glslang-dev glslc spirv-headers
# wget https://repo.radeon.com/amdgpu-install/latest/ubuntu/noble/amdgpu-install_7.2.4.70204-1_all.deb
# amdgpu-install --usecase=graphics,rocm,hip --vulkan=radv --opencl=rocr
# HIP_VISIBLE_DEVICES=1 \
# ROCM_PATH=/opt/rocm \
# HIP_PATH=/opt/rocm \
# HIP_PLATFORM=amd \
# HIP_DEVICE_LIB_PATH="$HIP_PATH/amdgcn/bitcode" \
# HIP_CXX="$HIP_PATH/llvm/bin/clang" \
# CMAKE_PREFIX_PATH="$ROCM_PATH/lib/cmake:$CMAKE_PREFIX_PATH" \
# cmake -B build -DGGML_HIP=ON -DCMAKE_HIP_FLAGS:STRING="-I$ROCM_PATH/include"
cmake -B build -DGGML_CUDA=ON
cmake --build build --config Release -j $(grep processor /proc/cpuinfo | wc -l)

restart-llama-server.sh

# use llama.cpp router mode
## see .local/bin/llama-server-restart.sh

# make build/bin/llama available on PATH
## see .local/bin/llama and llama-server symlinks

# CUDA init error on suspend/resume
## FIXED https://github.com/ggml-org/llama.cpp/issues/7218
