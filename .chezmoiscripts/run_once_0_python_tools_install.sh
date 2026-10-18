#!/bin/bash
# https://rob.cogit8.org/posts/2024-09-19-pyenv-to-uv/
# https://www.acervera.com/blog/2024/10/python-environment-poetry-pyenv-pipx/

# uv is now managed by mise (replaces asdf-uv); pinned in config.toml.

rm -fR ~/.pyenv
rm -fR ~/.local/pipx/venvs
rm -f ~/.config/fish/completions/pipx.fish
rm -f ~/.local/bin/uv ~/.local/bin/uvx # remove installer binaries, not mise managed anymore

export PATH="$HOME/.local/bin:$PATH"

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

uv python install 3 --default
