#!/usr/bin/fish

if mountpoint ~/.cache/huggingface/hub
    fish_add_path -Pg ~/go/bin ~/.asdf/shims # in case not yet set
    hf cache prune -y
end
