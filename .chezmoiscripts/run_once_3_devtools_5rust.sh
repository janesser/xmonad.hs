#!/bin/bash

# SAFEGUARD: apt-only script. `apt` and `snap` do not exist on a pacman/yay box,
# so skip silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo apt install -y rustup
rustup default stable

sudo snap remove rustup
exit 0
