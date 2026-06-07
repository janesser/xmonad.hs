#!/bin/bash
## FIXME https://github.com/pypa/pipx/issues/1069 pipx progressbar

sudo apt install -y pipx python3-opengl
register-python-argcomplete --shell fish pipx >~/.config/fish/completions/pipx.fish
pipx install -f jupyterlab
pipx inject -f jupyterlab jupyterlab-git jupyterlab_latex poetry-kernel
