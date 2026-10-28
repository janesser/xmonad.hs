#!/bin/bash

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo apt install -y python3-opengl

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

uv tool install --force -w jupyterlab-git,jupyterlab_latex,poetry-kernel jupyterlab
