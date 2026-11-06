#!/bin/bash

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo apt install -y podman containers-storage podman-compose
podman completion fish > ~/.config/fish/completions/podman.fish

# https://www.baeldung.com/OPS/PODMAN-PULL-IMAGE-DOCKER-HUB#pulling-images-without-fully-qualified-names
sudo sed -i 's/# unqualified-search-registries = \[.*\]/unqualified-search-registries = ["docker.io"]/' /etc/containers/registries.conf

# PODMAN fix permission trouble
## /var/run/docker.socket points where no one may reach

sudo groupadd docker # might not exist

# Ownership + mode of the podman socket directory and its sockets: handled by the
# root-owned wrapper (sudoers grants only the wrapper, never chmod/chown itself).
sudo /usr/local/bin/podman_socket_setup.sh

sudo usermod -a -G docker $USER

# PODMAN NVIDIA CDI setup

source "$CHEZMOI_SOURCE_DIR/commons/gpu-detect.sh"
if has_nvidia; then

    # No `sudo gpg` needed: dearmor as jan into /tmp, then `install` places it
    # root:root 0644 in one atomic call (same pattern as the wezterm keyring).
    curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey |\
        gpg --yes --dearmor > /tmp/nvidia-container-toolkit-keyring.gpg
    sudo install -o root -g root -m 0644 /tmp/nvidia-container-toolkit-keyring.gpg \
        /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg
    curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list \
        sed 's#deb https#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https#g' | \
        sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list

    sudo apt update
    sudo apt install -y nvidia-container-toolkit
    sudo systemctl daemon-reload

    if nvidia-ctk cdi list; then
        echo Found some cdi.
    else
        # `install -d` creates the directory with ownership+mode in one call
        # (replaces `sudo mkdir -p`); `install` copies the generated config into it
        # with root:root 0644 (replaces `sudo mv`, no intermediate file).
        sudo install -d -o root -g root -m 0755 /var/run/cdi \
            && nvidia-ctk cdi generate --mode csv --output=nvidia.yaml \
            && sudo install -o root -g root -m 0644 nvidia.yaml /var/run/cdi/nvidia.yaml \
            && nvidia-ctk cdi list # 0 and all
    fi
fi
