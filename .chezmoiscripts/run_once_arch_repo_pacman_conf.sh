#!/bin/bash
# run_once_arch_repo_pacman_conf.sh
#
# Point flowerpower's pacman at the Raspberry Pi repo server so
# `pacman -S hylki` installs the repo artifact instead of the AUR (yay).
#
#   /etc/pacman.conf       -> adds a [repo] section with
#                             Server = http://raspberrypi:8080/
#
# FLOWERPOWER ONLY: on any other host this logs and exits 0. The repo server is
# LAN-only, so the entry only makes sense on a box that can reach it.
#
# Sudo is used only for the pacman.conf edit (scoped NOPASSWD drop-in).

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"

REPO_URL="http://raspberrypi:8080/"
CONF="/etc/pacman.conf"

# --- 1. host gate (flowerpower only) -----------------------------------------
if [ "$(hostname)" != "flowerpower" ]; then
    echo "⏭️  $(hostname) is not flowerpower — the repo pacman.conf is flowerpower-only."
    exit 0
fi

echo "=> $SCRIPT_NAME: pointing pacman at the Pi repo on $(hostname)"

# --- 2. the pacman.conf entry (idempotent) ------------------------------------
if [ ! -f "$CONF" ]; then
    echo "=> $SCRIPT_NAME: ERROR: $CONF not found on $(hostname)"
    exit 1
fi

if grep -q "Server = ${REPO_URL}" "$CONF"; then
    echo "=> $SCRIPT_NAME: $CONF already has the Pi repo entry."
    exit 0
fi

if ! sudo bash -c "cat >> $CONF <<EOF

[repo]
Server = ${REPO_URL}
EOF"; then
    echo "=> $SCRIPT_NAME: ERROR: could not add the Pi repo entry to $CONF"
    exit 1
fi

echo "=> $SCRIPT_NAME: [repo] section added to $CONF"
echo "=> $SCRIPT_NAME: done. flowerpower: pacman -Sy && pacman -S hylki"
