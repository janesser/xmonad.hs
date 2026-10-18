#!/bin/bash

sudo apt install -y python3-opengl

eval "$(mise activate bash)"
mise install uv
mise use -g uv

uv tool install --force -w jupyterlab-git,jupyterlab_latex,poetry-kernel jupyterlab
