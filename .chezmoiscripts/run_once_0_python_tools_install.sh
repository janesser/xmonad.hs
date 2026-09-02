#!/bin/bash

# https://rob.cogit8.org/posts/2024-09-19-pyenv-to-uv/

# https://www.acervera.com/blog/2024/10/python-environment-poetry-pyenv-pipx/

rm -fR ~/.pyenv
rm -fR ~/.local/pipx/venvs
rm -f ~/.config/fish/completions/pipx.fish

asdf plugin add uv https://github.com/asdf-community/asdf-uv.git
asdf install uv latest

uv python install 3 --default