#!/bin/bash
#
# Report tool versions that mise considers out of date. READ-ONLY — this does
# NOT upgrade anything; it only prints what `mise upgrade` *would* do.
#
# Kept as a run hook so `chezmoi apply` surfaces update news without touching
# the tracked dotfiles (the old version ran `mise upgrade --all`, which drifted
# ~/.config/mise/config.toml out of sync with dot_config/mise/config.toml).
#
# Non-fatal: always exits 0 so an available update never breaks `apply`.

export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)" || {
    echo "mise not on PATH; run run_once_0_mise_install.sh first"
    exit 0
}

echo "==> mise: checking for out-of-date tools (read-only)..."
# --dry-run      : print what would be updated, change nothing
# --dry-run-code : exit non-zero when an update exists (we ignore it on purpose)
mise upgrade --dry-run --dry-run-code || true

echo "==> (updates are applied manually; this script reports only)"
exit 0
