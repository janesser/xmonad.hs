#!/bin/bash
#
# TeamViewer uninstaller — run during `cz apply` (called from
# .chezmoiscripts/run_9_cleanup_apt.sh). TeamViewer was removed from the repo
# (etc/apt/... source + keyring, and the install step in
# run_onchange_sudo_apt_sources.sh.tmpl); this script cleans it up from any
# machine that still has TeamViewer installed. Idempotent: safe to run every apply.

set -u

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- output helpers (define these before use; the colors above are their only deps) ---
say() { printf "%b%s%b\n" "$BLUE" "$1" "$NC"; }
ok()  { printf "%b%s%b\n" "$GREEN" "$1" "$NC"; }
warn(){ printf "%b%s%b\n" "$YELLOW" "$1" "$NC"; }
err() { printf "%b%s%b\n" "$RED" "$1" "$NC" >&2; }

# --- remove the package ---
if dpkg -s teamviewer >/dev/null 2>&1; then
    say "Removing teamviewer package…"
    sudo apt remove --purge -y teamviewer || warn "apt remove returned non-zero (continuing)"
else
    ok "teamviewer package not installed — nothing to remove"
fi

# --- remove the apt source + keyring ---
say "Removing TeamViewer apt source and keyring…"
sudo rm -f /etc/apt/sources.list.d/teamviewer.list
sudo rm -f /etc/apt/sources.list.d/teamviewer.sources
sudo rm -f /etc/apt/keyrings/teamviewer-keyring.gpg

# --- remove leftover install dir and configs ---
say "Removing TeamViewer install dir and config…"
sudo rm -rf /opt/teamviewer
sudo rm -rf /root/.teamviewer
rm -rf ~/.teamviewer ~/.config/teamviewer ~/.cache/teamviewer 2>/dev/null || true

# --- purge orphaned dependencies ---
say "Running apt autoremove…"
sudo apt autoremove --purge -y || true

ok "TeamViewer uninstaller finished."
