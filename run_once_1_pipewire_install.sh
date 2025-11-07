#!/bin/bash

if [ "$CHEZMOI_ARCH" = "amd64" ]; then
    sudo apt install -y pipewire-audio easyeffects blueman
    sudo mkdir -p /etc/pipewire
    sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systemctl restart --user pipewire
else
    echo $CHEZMOI_ARCH not supported for pipewire setup.
fi
