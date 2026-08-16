#!/usr/bin/fish

fish_add_path -g $HOME/.local/bin $PATH

function hf --description "wrapper of uvx hf in venv of .comfy"
    if ! mountpoint ~/.cache/huggingface/hub
        exit 1
    end
    source ~/.comfy/bin/activate.fish
    uvx hf $argv
    deactivate
end