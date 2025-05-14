#!/bin/bash

sudo apt install pipewire-audio easyeffects blueman
sudo mkdir -p /etc/pipewire
sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
systemctl restart --user pipewire