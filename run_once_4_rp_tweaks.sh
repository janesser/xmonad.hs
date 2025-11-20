#!/bin/bash

# https://thelinuxcode.com/best-video-player-raspberry-pi/

if [ "$CHEZMOI_ARCH" = "arm64" ]; then
    sudo apt install -y mesa-vdpau-drivers
    sudo sed -i '/^gpu_mem=/d' /boot/firmware/config.txt
    sudo sed -i '/^display_overlay=/d' /boot/firmware/config.txt
    sudo sed -i '/^core_freq=/d' /boot/firmware/config.txt
    echo 'gpu_mem=512' | sudo tee -a /boot/firmware/config.txt
    echo 'disable_overlay=0' | sudo tee -a /boot/firmware/config.txt
    echo 'core_freq=550' | sudo tee -a /boot/firmware/config.txt
fi
