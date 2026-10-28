#!/bin/bash

# SAFEGUARD: apt-only script. `extrepo` and `apt` are apt-flavoured — on a
# pacman/yay box neither exists, so skip silently instead of failing the whole
# `cz apply`.
command -v apt >/dev/null || exit 0

sudo extrepo enable vscodium
sudo apt update
sudo apt install -y codium
