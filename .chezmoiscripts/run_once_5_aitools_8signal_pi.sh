#!/bin/bash
# run_once_5_aitools_8signal_pi.sh
#
# One-time install for the Signal <-> pi persistent agent on raspberrypi.
# Managed by chezmoi: `run_once_*` runs once per machine, `run_*` (see
# run_5_signal_update.sh) is what checks for signal-cli updates on every apply.
#
# What it installs/verifies:
#   1. signal-cli        ~/.local/lib/signal-cli-<VERSION>/ + symlink to
#                       ~/.local/bin/signal-cli (per-user, not /opt)
#                       (moves the one-time /opt install into place if needed)
#   2. signal-receive-loop.sh   ~/.local/bin/ — managed by chezmoi as a template
#   3. the user-space systemd units from the repo source
#
# On any other host this script logs and exits 0.

set -euo pipefail

CHEZMOI_SOURCE_DIR="${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}"

log_info() { echo "[INFO] $*"; }
log_ok()   { echo "[OK]   $*"; }
log_warn() { echo "[WARN] $*"; }
log_error(){ echo "[ERROR] $*"; }

# --- host gate (raspberrypi only) ---------------------------------------------
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "$(hostname) is not raspberrypi — signal-pi only runs there."
    exit 0
fi

# --- 1. Java 25+ --------------------------------------------------------------
if ! command -v java &>/dev/null; then
    log_error "Java is not installed. signal-cli requires Java 25+."
    exit 1
fi
java_version=$(java -version 2>&1 | head -1 | sed -E 's/.*version "([0-9]+).*/\1/')
if [ "$java_version" -lt 25 ]; then
    log_error "Java 25+ required, found version $java_version."
    exit 1
fi
log_ok "Java $java_version found."

VERSION_INSTALLED=""
if command -v signal-cli &>/dev/null; then
    VERSION_INSTALLED=$(signal-cli --version 2>/dev/null | grep -oE '[0-9.]+' | head -1 || true)
fi
log_info "signal-cli version: ${VERSION_INSTALLED:-<not found>}"

# Only download the release when a newer version exists on GitHub.
VERSION_LATEST=$(curl -sL -o /dev/null -w '%{url_effective}' \
    https://github.com/AsamK/signal-cli/releases/latest 2>/dev/null | sed 's|.*/v||' || true)

if [ -n "$VERSION_LATEST" ] && [ -n "$VERSION_INSTALLED" ]; then
    if [[ "$VERSION_LATEST" > "$VERSION_INSTALLED" ]]; then
        log_warn "newer release $VERSION_LATEST available — handled by run_5_signal_update.sh on next apply."
    else
        log_info "signal-cli is up to date (${VERSION_INSTALLED} vs ${VERSION_LATEST})."
    fi
else
    log_warn "could not compare versions (installed=$VERSION_INSTALLED, latest=$VERSION_LATEST)."
fi

# --- 3. units (user-space, from the repo source) --------------------------------
UNIT_RECEIVE_SRC="${CHEZMOI_SOURCE_DIR}/dot_config/systemd/user/signal-receive.service"
UNIT_PI_SRC="${CHEZMOI_SOURCE_DIR}/dot_config/systemd/user/signal-pi.service"

for unit_src in "$UNIT_RECEIVE_SRC" "$UNIT_PI_SRC"; do
    [ -f "$unit_src" ] || { log_error "unit source missing: $unit_src"; exit 1; }
done

systemctl --user daemon-reload && log_ok "user-unit daemon-reload OK"

for unit in signal-receive.service signal-pi.service; do
    if systemctl --user status --quiet "$unit" 2>/dev/null; then
        log_ok "unit $unit is loaded"
    else
        log_warn "unit $unit is not loaded — check it was installed via chezmoi."
    fi
done

echo ""
log_ok "signal-pi install complete."
echo "  signal-cli: $SIGNAL_CLI_LINK"
echo "  loop script: ~/.local/bin/signal-receive-loop.sh (chezmoi)"
echo "  units:      ~/.config/systemd/user/{signal-receive,signal-pi}.service (chezmoi)"
echo "  update later: run_5_signal_update.sh runs on every chezmoi apply"
