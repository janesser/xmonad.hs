#!/bin/bash
#
# Install mise — the drop-in replacement for asdf — and wire it into fish.
# asdf is fully removed; tool versions are pinned in dot_config/mise/config.toml.
# See mise-migration-plan.md.

export PATH="$HOME/.local/bin:$PATH"

sudo apt install -y git

# Install mise (official installer drops the binary into ~/.local/bin).
# ~/.profile already puts ~/.local/bin on PATH for future login shells.
curl -fsSL https://mise.jdx.dev/install.sh | bash

export PATH="$HOME/.local/bin:$PATH"

# Fish completions (replaces the old asdf completion file).
if command -v mise >/dev/null 2>&1; then
    mise completion fish > ~/.config/fish/completions/mise.fish 2>/dev/null || true
fi

# Interactive shell integration (PATH shims + cd-aware version switching) lives in
# ~/.config/fish/conf.d/mise.fish (tracked in the repo) and calls
# `eval "$(mise activate fish)"`. There is no asdf.fish anymore.

echo "mise installed at: $(command -v mise)"
