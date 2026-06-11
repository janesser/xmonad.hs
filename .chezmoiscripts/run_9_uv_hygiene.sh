#!/usr/bin/fish

pyenv init - fish | source

pipx upgrade uv
uv cache clean