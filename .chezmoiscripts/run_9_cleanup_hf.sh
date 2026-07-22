#!/usr/bin/fish

if mountpoint ~/.cache/huggingface/hub
    hf cache prune -y
end
