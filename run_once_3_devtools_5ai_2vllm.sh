#!/bin/bash

sudo apt install -y pipx
pipx install -f uv
uv venv --allow-existing
uv pip install -U vllm
uv pip install -U huggingface_hub

# vllm serve "shahidul034/Qwen2.5-0.5B-search-think-answer" --max-model-len 1K --gpu-memory-utilization 0.7 --max_num_seqs 1 --api-key vllm
