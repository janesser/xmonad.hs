#!/bin/bash

# SAFEGUARD: nala-only script. `nala` is an apt-flavoured package manager — on a
# pacman/yay box it does not exist, so skip silently instead of failing the whole
# `cz apply`.
command -v nala >/dev/null || exit 0

sudo nala full-upgrade -y
