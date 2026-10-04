#!/bin/bash

# PODMAN NVIDIA CDI setup.

source "$HOME/.local/share/gpu.func"
if ! has_nvidia; then
    echo "$(basename $0): No NVIDIA GPU detected, skipping podman-nvidia setup..."
    # Remove only what *this* script would have installed. Deliberately NOT
    # touching nvidia-cuda-toolkit (installed elsewhere for CUDA builds).
    sudo apt remove -y --purge nvidia-container-toolkit gst-plugin-nvidia-container 2>/dev/null
    sudo apt autoremove -y
    sudo rm -f /etc/apt/sources.list.d/nvidia-container-toolkit.list
    sudo rm -f /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg
    sudo rm -f /var/run/cdi/nvidia.yaml 2>/dev/null
    sudo systemctl daemon-reload
    exit 0
fi

sudo apt install -y podman containers-storage podman-compose
podman completion fish > ~/.config/fish/completions/podman.fish

# https://www.baeldung.com/OPS/PODMAN-PULL-IMAGE-DOCKER-HUB#pulling-images-without-fully-qualified-names
sudo sed -i 's/# unqualified-search-registries = \[.*\]/unqualified-search-registries = ["docker.io"]/' /etc/containers/registries.conf

curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
  && curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list | \
    sed 's#deb https#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https#g' | \
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

# PODMAN fix permission trouble
## /var/run/docker.socket points where no one may reach

sudo groupadd docker # might not exist

sudo chown root:docker /run/podman
sudo chmod 750 /run/podman # 700 before
# glob expands in this shell before sudo sees it, so no wildcard reaches sudo.
sudo chown root:docker /run/podman/*.sock
sudo chmod 660 /run/podman/*.sock # 600 before

sudo usermod -a -G docker $USER
