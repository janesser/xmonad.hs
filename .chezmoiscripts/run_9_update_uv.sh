#!/bin/bash
# uv is managed by mise (replaces asdf); shims on PATH via activate.
export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)"

# uv self update # only binary installer
uv tool upgrade --all
