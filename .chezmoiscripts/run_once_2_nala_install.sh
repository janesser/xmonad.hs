#!/bin/bash

# SAFEGUARD: nala-only script. `nala` is an apt-flavoured package manager — on a
# pacman/yay box it does not exist, so skip silently instead of failing the whole
# `cz apply`.
command -v nala >/dev/null || exit 0

sudo apt install -y nala
#sudo nala fetch
sudo nala update

nala --install-completion bash

if command -v fish >/dev/null
then
    fish -c "nala --install-completion fish"
fi
