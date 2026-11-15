#!/bin/bash

# run_once_5_herdr_install.sh
#
# Install herdr, plus the agent integrations that let herdr see and resume
# sessions in its panes: pi and OpenCode. `herdr integration install` is
# idempotent — safe to run on every apply.

curl -fsSL https://herdr.dev/install.sh | sh

mkdir -p ~/.pi/agent/extensions
herdr integration install pi

# --- OpenCode ---------------------------------------------------------------
# herdr can manage an OpenCode session running inside one of its panes (local
# or attached to a remote machine) only if the plugin is installed in the box's
# OpenCode config. This box has one only when `~/.config/opencode` exists —
# that directory is rendered to every host by chezmoi (dot_config/opencode). On
# boxes without it, herdr has nothing to manage so this is a no-op.
if command -v herdr >/dev/null 2>&1 && [ -d "$HOME/.config/opencode" ]; then
    echo "🔌 OpenCode config present on $(hostname) — installing herdr integration for OpenCode."
    if ! herdr integration install opencode >/dev/null 2>&1; then
        echo "⚠️  herdr integration install opencode failed — run it by hand:"
        echo "     herdr integration install opencode"
    else
        echo "✅ herdr-OpenCode integration installed on $(hostname)."
    fi
else
    echo "⏭️  No OpenCode config on $(hostname) — skipping herdr-OpenCode integration."
fi
