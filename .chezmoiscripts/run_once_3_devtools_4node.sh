#!/bin/bash
#
# nodejs via mise (replaces asdf). Pinned to 24.20.0 in config.toml.

rm -fR ~/.nvm

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-node.sh"