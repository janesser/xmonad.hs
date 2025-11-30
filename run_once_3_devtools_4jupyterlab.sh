#!/bin/bash

sudo apt install -y pipx python3-opengl

# jupyter-lab stop
# pipx uninstall jupyterlab

pipx install poetry
pipx install jupyterlab
pipx inject jupyterlab jupyterlab-git jupyterlab_latex poetry-kernel
