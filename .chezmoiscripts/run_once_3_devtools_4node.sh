#!/bin/bash
#
# nodejs via mise (replaces asdf). Pinned to 24.20.0 in config.toml.

rm -fR ~/.nvm

export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)"
mise install nodejs
