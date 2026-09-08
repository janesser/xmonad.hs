#!/bin/bash
#
# java + maven via mise (replaces asdf). Pinned in config.toml.
# gradle is intentionally dropped: it was added before but never pinned.

rm -rf ~/.sdkman
rm -f ~/.config/fish/completions/sdk.fish
rm -f ~/.config/fish/conf.d/sdk.fish

export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)"
mise install java
mise install maven
