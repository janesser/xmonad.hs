#!/bin/bash
#
# Olla service uninstaller — run during `cz apply` (called from
# run_9_cleanup_apt.sh). The Olla systemd unit (`olla.service`) is removed from
# this repo; this script cleans any machine that still has it installed. Idempotent:
# safe to run every apply.
#
# What it cleans:
#   1. `/etc/systemd/system/olla.service` (the system unit — stops, disables, deletes)
#   2. `~/.config/olla/config.yaml` (the Olla config, now orphaned)
#   3. the Olla binary left in the mise install dir (best-effort, no sudo)
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in
# (systemctl, rm, chmod).

set -u

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- output helpers ---
say() { printf "%b%s%b\n" "$BLUE" "$1" "$NC"; }
ok()  { printf "%b%s%b\n" "$GREEN" "$1" "$NC"; }
warn(){ printf "%b%s%b\n" "$YELLOW" "$1" "$NC"; }
err() { printf "%b%s%b\n" "$RED" "$1" "$NC" >&2; }

UNIT_NAME=olla.service
UNIT_DEST="/etc/systemd/system/${UNIT_NAME}"
OLLADA_DIR="$HOME/.config/olla"
OLLADA_CONF="$OLLADA_DIR/config.yaml"
OLLAMA_BIN_SRC="$HOME/.local/share/mise/installs/github-thushan-olla/latest/olla"

# --- 1. stop + disable the unit (if it exists) -------------------------------
if [ -f "${UNIT_DEST}" ]; then
    say "Found ${UNIT_NAME} — stopping and disabling it…"
    sudo systemctl stop "${UNIT_NAME}" 2>/dev/null || warn "systemctl stop returned non-zero (continuing)"
    sudo systemctl disable "${UNIT_NAME}" 2>/dev/null || warn "systemctl disable returned non-zero (continuing)"
    sudo systemctl daemon-reload 2>/dev/null || warn "systemctl daemon-reload returned non-zero (continuing)"
else
    ok "Unit ${UNIT_DEST} not present — nothing to stop"
fi

# --- 2. remove the unit file ------------------------------------------------
if [ -f "${UNIT_DEST}" ]; then
    sudo rm -f "${UNIT_DEST}"
    ok "Removed ${UNIT_DEST}"
else
    ok "Unit file already absent — nothing to remove"
fi

# --- 3. remove the Olla config dir (orphaned, no service to serve) -----------
if [ -d "${OLLADA_DIR}" ]; then
    say "Removing Olla config dir ${OLLADA_DIR}…"
    rm -rf "${OLLADA_DIR}"
    ok "Removed ${OLLADA_DIR}"
else
    ok "Olla config dir ${OLLADA_DIR} not present — nothing to remove"
fi

# --- 4. remove the Olla binary (mise install dir) ----------------------------
if [ -f "${OLLAMA_BIN_SRC}" ]; then
    say "Removing Olla binary from the mise install dir…"
    rm -f "${OLLAMA_BIN_SRC}"
    ok "Removed ${OLLAMA_BIN_SRC}"
else
    ok "Olla binary not present in the mise install dir — nothing to remove"
fi

ok "Olla service uninstaller finished."
