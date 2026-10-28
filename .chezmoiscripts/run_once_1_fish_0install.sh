#!/bin/bash

# SAFEGUARD: apt-only script. `add-apt-repository` and `apt` do not exist on a
# pacman/yay box, so skip silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo add-apt-repository ppa:fish-shell/release-4
sudo apt update
sudo apt install -y fish
chsh -s $(which fish)
