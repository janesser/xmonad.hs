#!/bin/bash

if mountpoint ~/.cache/huggingface/hub; then
    hf cache prune -y
fi
