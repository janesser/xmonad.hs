#!/bin/bash
# run_once_5_aitools_9repo_server.sh
#
# Bring the pacman repo server on the Raspberry Pi up and keep it running:
#
#   /srv/repo                    -> the repo dir (created, jan-owned)
#   ~/.config/systemd/user/repo-server.service -> the userspace unit (from the
#                                                 repo source, rendered by
#                                                 chezmoi)
#   http://<pi>:8080/            -> the repo, served by python3 http.server
#
# PI ONLY: on any other host this logs and exits 0. The repo dir needs root to
# create (it sits under /srv), everything else is userspace: no nginx, no
# system packages, no port 80.
#
# The unit is a chezmoi target, so it is rendered into
# ~/.config/systemd/user/ and only enabled here. `cz apply` runs this script
# once, then the unit keeps the server alive across reboots.
#
# Sudo is used only for the /srv/repo creation (scoped NOPASSWD drop-in).

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"
SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

REPO_DIR="/srv/repo"
REPO_PORT=8080
UNIT="repo-server.service"
UNIT_SRC="${SRC_DIR}/dot_config/systemd/user/${UNIT}"
UNIT_DEST="${HOME}/.config/systemd/user/${UNIT}"

# --- 1. host gate (Pi only) ---------------------------------------------------
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "⏭️  $(hostname) is not the Pi — the repo server is Pi-only."
    exit 0
fi

echo "=> $SCRIPT_NAME: setting up the repo server on $(hostname)"

# --- 2. the repo dir (needs root to create under /srv) -------------------------
# The build host rsyncs into this dir as jan@<pi>, so it must exist and be
# owned by jan.
if [ ! -d "$REPO_DIR" ]; then
    echo "=> $SCRIPT_NAME: creating $REPO_DIR (jan-owned)"
    if ! sudo mkdir -p "$REPO_DIR"; then
        echo "=> $SCRIPT_NAME: ERROR: could not create $REPO_DIR"
        exit 1
    fi
    if ! sudo chown jan:jan "$REPO_DIR"; then
        echo "=> $SCRIPT_NAME: ERROR: could not own $REPO_DIR to jan"
        exit 1
    fi
    echo "=> $SCRIPT_NAME: $REPO_DIR ready"
else
    echo "=> $SCRIPT_NAME: $REPO_DIR exists"
fi

# --- 3. the userspace unit (already rendered by chezmoi, just enable it) -------
if [ ! -f "$UNIT_DEST" ]; then
    echo "=> $SCRIPT_NAME: ERROR: $UNIT not rendered by chezmoi at $UNIT_DEST"
    exit 1
fi

systemctl --user enable "$UNIT" --now 2>/dev/null || {
    echo "=> $SCRIPT_NAME: ERROR: could not enable $UNIT"
    exit 1
}
echo "=> $SCRIPT_NAME: $UNIT enabled and started"

# --- 4. verify ---------------------------------------------------------------
if curl -fsS -m 5 "http://127.0.0.1:${REPO_PORT}/" >/dev/null 2>&1; then
    echo "=> $SCRIPT_NAME: repo server up at http://<pi>:${REPO_PORT}/"
else
    echo "=> $SCRIPT_NAME: WARNING: repo server not responding on :${REPO_PORT}"
    exit 1
fi

echo "=> $SCRIPT_NAME: done. next: build-hylki.sh on cyberkleiber"
