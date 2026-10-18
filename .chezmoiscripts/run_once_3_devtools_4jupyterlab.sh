#!/bin/bash

sudo apt install -y python3-opengl

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

uv tool install --force -w jupyterlab-git,jupyterlab_latex,poetry-kernel jupyterlab
