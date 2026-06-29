#!/bin/bash

lsmod | grep nvidia
if [[ $? -ne 0 ]]; then
    echo "$(basename $0): No nvidia module loaded in kernel, skipping..."
    exit 0
fi

uv venv --allow-existing ~/.comfy
source ~/.comfy/bin/activate

uv pip install comfy-cli

# comfy-ui setup & install

comfy setup --where local -y
cd ~/.comfy
comfy install --restore --nvidia --cuda-version 12.4 --fast-deps --version=latest

comfy manager enable-gui
comfy manager uv-compile-default true


# get flux2-klein
## https://docs.comfy.org/tutorials/flux/flux-2-klein

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/diffusion_models/flux-2-klein-base-4b.safetensors
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/diffusion_models/flux-2-klein-4b.safetensors

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/text_encoders/qwen_3_4b.safetensors
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/text_encoders/qwen_3_4b_fp4_flux2.safetensors

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/vae/flux2-vae.safetensors

# Softlink all files under snapshot to models
for folder in ~/.cache/huggingface/hub/models--Comfy-Org--vae-text-encorder-for-flux-klein-4b/snapshots/*/split_files/*; do
    #echo $folder
    basefolder=`basename $folder`
    for item in `ls $folder`; do
        #echo $item
        cd ~/comfy/ComfyUI/models/$basefolder
        ln -sf "$folder/$item"
    done
done
echo all linked nicely.

curl -o ~/comfy/ComfyUI/user/default/workflows/image_flux2_klein_text_to_image.json \
  https://raw.githubusercontent.com/Comfy-Org/workflow_templates/refs/heads/main/templates/image_flux2_klein_text_to_image.json

# comfy launch