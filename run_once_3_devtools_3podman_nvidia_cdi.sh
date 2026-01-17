#!/bin/bash

sudo apt install -y podman containers-storage
podman completion fish > ~/.config/fish/completions/podman.fish

curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
  && curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list | \
    sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
    sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list

sudo apt update
sudo apt install -y nvidia-container-toolkit
sudo systemctl daemon-reload

if nvidia-ctk cdi list; then
    echo Found some cdi.
else
    nvidia-ctk cdi generate --mode csv --output=nvidia.yaml
    sudo mkdir -p /var/run/cdi
    sudo mv nvidia.yaml /var/run/cdi/
    nvidia-ctk cdi list # 0 and all
fi
