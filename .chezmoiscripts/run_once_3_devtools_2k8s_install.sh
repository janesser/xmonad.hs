#!/bin/bash
#
# kubectl + helm via mise (replaces asdf). Pinned in config.toml.

export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)"
mise install kubectl
mise install helm
