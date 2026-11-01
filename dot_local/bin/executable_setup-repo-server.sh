#!/usr/bin/env bash
# executable_setup-repo-server.sh — userspace pacman repo server on the
# Raspberry Pi.
#
#   executable_setup-repo-server.sh
#
# Serves the pacman repo (built by executable_build-hylki.sh and rsynced to
# /srv/repo) over HTTP in userspace: python3 -m http.server on port 8080,
# started as a systemd user unit (dot_config/systemd/user/repo-server.service).
# No nginx, no system packages, no port 80, no root.
#
# The repo must already exist at /srv/repo (rsynced from the build host) — this
# script only creates it if needed, installs the unit and starts the server.
#
# sudo is used (scoped NOPASSWD drop-in) only for the /srv/repo creation.

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"

# --- hostname guard: only meaningful on the Pi ---
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "=> $SCRIPT_NAME: only intended for the Raspberry Pi (raspberrypi)"
    echo "=> $SCRIPT_NAME: run on the Pi, not on $(hostname)"
    exit 1
fi

REPO_DIR="${REPO_DIR:-/srv/repo}"
REPO_PORT=8080
UNIT="repo-server.service"
UNIT_DEST="${HOME}/.config/systemd/user/${UNIT}"

echo "=> $SCRIPT_NAME: serving $REPO_DIR on the Pi (port $REPO_PORT)"

# --- 1. the repo dir (needs root to create under /srv) -------------------------
if [ ! -d "$REPO_DIR" ]; then
    echo "=> $SCRIPT_NAME: creating $REPO_DIR (jan-owned)"
    sudo mkdir -p "$REPO_DIR" && sudo chown jan:jan "$REPO_DIR" || {
        echo "=> $SCRIPT_NAME: ERROR: could not create $REPO_DIR"
        exit 1
    }
else
    echo "=> $SCRIPT_NAME: $REPO_DIR exists"
fi

# --- 2. the userspace unit (renders via chezmoi, enabled here) -----------------
if [ ! -f "$UNIT_DEST" ]; then
    echo "=> $SCRIPT_NAME: ERROR: $UNIT not present at $UNIT_DEST"
    exit 1
fi

systemctl --user enable "$UNIT" --now || {
    echo "=> $SCRIPT_NAME: ERROR: could not enable $UNIT"
    exit 1
}
echo "=> $SCRIPT_NAME: $UNIT enabled and started"

# --- 3. verify -----------------------------------------------------------------
if curl -fsS -m 5 "http://127.0.0.1:${REPO_PORT}/" >/dev/null 2>&1; then
    echo "=> $SCRIPT_NAME: repo server up at http://<pi-ip>:${REPO_PORT}/"
else
    echo "=> $SCRIPT_NAME: WARNING: repo server not responding on :${REPO_PORT}"
    exit 1
fi
