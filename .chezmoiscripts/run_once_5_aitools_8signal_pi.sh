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

# --- 2. signal-cli (per-user install: ~/.local/lib + ~/.local/bin) -----------
SIGNAL_CLI_LINK="$HOME/.local/bin/signal-cli"   # executable, in PATH
SIGNAL_CLI_DIR="$HOME/.local/lib/signal-cli"    # installed releases

# Version of the binary that is actually reachable right now. `signal-cli` in
# PATH may still point at a stale system symlink (root-owned /usr/local/bin) or
# an already-moved per-user copy — follow whatever resolves to the real file.
VERSION_INSTALLED=""
if command -v signal-cli &>/dev/null; then
    VERSION_INSTALLED=$(signal-cli --version 2>/dev/null | grep -oE '[0-9.]+' | head -1 || true)
fi
if [ -z "$VERSION_INSTALLED" ] && [ -L /usr/local/bin/signal-cli ]; then
    # stale system symlink: /opt/signal-cli-X.Y.Z/bin/signal-cli → version from path
    VERSION_INSTALLED=$(readlink /usr/local/bin/signal-cli | sed 's|.*/signal-cli-\([0-9.]*\)/bin.*|\1|')
fi
if [ -z "$VERSION_INSTALLED" ]; then
    # nothing to follow: look for an existing per-user release copy
    VERSION_INSTALLED=$(ls -d "$SIGNAL_CLI_DIR"/signal-cli-* 2>/dev/null | sed "s|.*/signal-cli-\([0-9.]*\)|\1|" | head -1)
fi
log_info "signal-cli version: ${VERSION_INSTALLED:-<not found>}"

if [ -L "$SIGNAL_CLI_LINK" ] && [ "$(readlink -f "$SIGNAL_CLI_LINK")" = "${SIGNAL_CLI_DIR}/signal-cli-${VERSION_INSTALLED}/bin/signal-cli" ]; then
    log_ok "signal-cli already installed per-user: $SIGNAL_CLI_LINK -> $(readlink -f "$SIGNAL_CLI_LINK")"
else
    # The one-time /opt install (from the pi-signal setup wizard) is in place —
    # move it into the per-user layout so no root-owned paths are needed.
    if [ -n "$VERSION_INSTALLED" ] && [ -f "/opt/signal-cli-${VERSION_INSTALLED}/bin/signal-cli" ]; then
        log_info "moving /opt/signal-cli-${VERSION_INSTALLED} to ~/.local/lib/signal-cli-${VERSION_INSTALLED}"
        mkdir -p "$SIGNAL_CLI_DIR"
        sudo mv "/opt/signal-cli-${VERSION_INSTALLED}" "$SIGNAL_CLI_DIR/signal-cli-${VERSION_INSTALLED}"
    fi

    mkdir -p "$HOME/.local/bin"
    rm -f "$SIGNAL_CLI_LINK"
    sudo rm -f /usr/local/bin/signal-cli   # system symlink points at the old /opt copy
    ln -sf "${SIGNAL_CLI_DIR}/signal-cli-${VERSION_INSTALLED}/bin/signal-cli" "$SIGNAL_CLI_LINK"
    log_ok "signal-cli installed per-user: $SIGNAL_CLI_LINK -> $(readlink -f "$SIGNAL_CLI_LINK")"
fi

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
