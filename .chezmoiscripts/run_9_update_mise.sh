#!/bin/bash
#
# Update tool versions managed by mise.
# Replaces run_9_update_asdf.sh. mise resolves the latest version within each
# pinned range; we never rewrite the pins here (no --bump).

export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)" || {
    echo "mise not on PATH; run run_once_0_mise_install.sh first"
    exit 0
}

mise upgrade --all || echo "warning: some upgrades failed"

# nektos/act is installed via go (not managed by mise), so keep it fresh too.
if command -v go >/dev/null 2>&1; then
    export PATH="$HOME/go/bin:$PATH"
    go install github.com/nektos/act@latest || echo "warning: act update failed"
fi
