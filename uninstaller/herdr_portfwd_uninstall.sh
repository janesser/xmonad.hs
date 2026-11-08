#!/bin/bash
#
# herdr-portfwd uninstaller — run during `cz apply` (called from
# .chezmoiscripts/run_9_cleanup_herdr_portfwd.sh). The herdr-portfwd plugin
# (miko-misa/herdr-portfwd), its CLI, and its config are removed from any box
# that got them installed. Idempotent: safe to run on every apply.
#
# What it cleans:
#   1. `~/.config/herdr/plugins/config/portfwd` (plugin config)
#   2. `~/.config/herdr/plugins/github-installations/portfwd-*` (plugin install dir)
#   3. `~/.local/bin/herdr-portfwd` (the CLI, installed by the plugin)
#   4. `~/.config/herdr-portfwd/{config.toml,hosts.toml}` (rendered config)
#
# Sudo is not needed: everything here is owned by $USER.

set -euo pipefail

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- output helpers -------------------------------------------------------------
say() { printf "%b%s%b\n" "$BLUE" "$1" "$NC"; }
ok()  { printf "%b%s%b\n" "$GREEN" "$1" "$NC"; }
warn(){ printf "%b%s%b\n" "$YELLOW" "$1" "$NC"; }
err() { printf "%b%s%b\n" "$RED" "$1" "$NC" >&2; }

# --- 1. herdr gate --------------------------------------------------------------
if ! command -v herdr >/dev/null; then
    ok "herdr is not installed on $(hostname) — nothing to remove."
    exit 0
fi

echo "$(basename "$0"): cleaning herdr-portfwd on $(hostname) ..."

# --- 2. uninstall the plugin ----------------------------------------------------
if ! herdr plugin uninstall portfwd >/dev/null 2>&1; then
    warn "herdr plugin uninstall failed — removing leftovers manually"
else
    ok "herdr plugin 'portfwd' uninstalled."
fi

# --- 3. remove leftover plugin dirs ---------------------------------------------
rm -rf "${HOME}/.config/herdr/plugins/config/portfwd" || true
rm -rf "${HOME}/.config/herdr/plugins/github-installations/portfwd-"* || true
ok "herdr plugin config/installs removed."

# --- 4. remove the CLI ----------------------------------------------------------
if [ -x "${HOME}/.local/bin/herdr-portfwd" ]; then
    say "Removing herdr-portfwd CLI..."
    rm -f "${HOME}/.local/bin/herdr-portfwd"
    ok "Removed ${HOME}/.local/bin/herdr-portfwd"
else
    ok "herdr-portfwd CLI not present — nothing to remove."
fi

# --- 5. remove the config dir ---------------------------------------------------
if [ -d "${HOME}/.config/herdr-portfwd" ]; then
    say "Removing herdr-portfwd config dir..."
    rm -rf "${HOME}/.config/herdr-portfwd"
    ok "Removed ${HOME}/.config/herdr-portfwd"
else
    ok "herdr-portfwd config dir not present — nothing to remove."
fi

ok "herdr-portfwd cleanup finished."
