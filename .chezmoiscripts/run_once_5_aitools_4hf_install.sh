#!/bin/bash
#
# Install the Hugging Face CLI (`hf`) as a proper uv tool.
#
# This replaces the old `uvx hf` wrapper that used to live in
# ~/.config/fish/conf.d/hf.fish (removed — now obsolete: `uv tool install hf`
# puts a real `hf` binary on ~/.local/bin, which is already on PATH, so `hf`
# runs directly with no wrapper).
#
# Run via `mise exec` so the pinned uv (see dot_config/mise/config.toml) is
# used and the install is reproducible across machines. `mise exec` works
# without shell activation. Idempotent — re-running just re-resolves.

export PATH="$HOME/.local/bin:$PATH"

mise exec -- uv tool install hf
