#!/bin/bash
# run_once_arch_repo_pacman_conf.sh
#
# Point flowerpower's pacman at the Raspberry Pi repo server so
# `pacman -S hylki` installs the repo artifact instead of the AUR (yay).
#
#   /etc/pacman.d/conf.d/lan_repo.conf   -> the [repo] section with
#                                        Server = http://raspberrypi:8080/
#                                        SigLevel = Never (the package is
#                                        unsigned, so pacman must not ask for
#                                        `hylki-bin-*.pkg.tar.zst.sig`)
#
# FLOWERPOWER ONLY: on any other host this logs and exits 0. The repo server is
# LAN-only, so the entry only makes sense on a box that can reach it.
#
# Sudo is used only for the file write (scoped NOPASSWD drop-in).

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"
CONF_DIR="/etc/pacman.d/conf.d"
CONF_DEST="${CONF_DIR}/lan_repo.conf"
CONF_SRC="${CHEZMOI_SOURCE_DIR:-.}/etc/pacman.d/conf.d/lan_repo.conf"
CONF="/etc/pacman.conf"

# --- 1. host gate (flowerpower only) -----------------------------------------
if [ "$(hostname)" != "flowerpower" ]; then
    echo "⏭️  $(hostname) is not flowerpower — the repo pacman.conf is flowerpower-only."
    exit 0
fi

echo "=> $SCRIPT_NAME: pointing pacman at the Pi repo on $(hostname)"

# --- 2. the pacman drop-in (renders the [repo] section) -----------------------
if [ ! -f "$CONF_SRC" ]; then
    echo "=> $SCRIPT_NAME: ERROR: $CONF_SRC not found on $(hostname)"
    exit 1
fi

mkdir -p "$CONF_DIR" || {
    echo "=> $SCRIPT_NAME: ERROR: could not create $CONF_DIR"
    exit 1
}

sudo install -o root -g root -m 0644 "$CONF_SRC" "$CONF_DEST" || {
    echo "=> $SCRIPT_NAME: ERROR: could not install $CONF_DEST"
    exit 1
}
echo "=> $SCRIPT_NAME: $CONF_DEST installed"

# --- 3. remove the legacy [repo] section appended to /etc/pacman.conf ---------
# A section must not be defined in both the main config and a drop-in, so the
# entry that `cz apply` used to append there has to go.
if grep -q "^\[repo\]" "$CONF"; then
    echo "=> $SCRIPT_NAME: removing the legacy [repo] section from $CONF"
    sudo bash -c "python3 - <<'EOF'
import re
path = '/etc/pacman.conf'
with open(path) as f:
    content = f.read()
content = re.sub(r'^\n?\[repo\]\n(?:Server.*\n)+', '', content, flags=re.M)
with open(path, 'w') as f:
    f.write(content)
EOF" || {
        echo "=> $SCRIPT_NAME: ERROR: could not remove the legacy [repo] section from $CONF"
        exit 1
    }
else
    echo "=> $SCRIPT_NAME: $CONF has no [repo] section"
fi

# --- 4. verify ---------------------------------------------------------------
if grep -q "Server = http://raspberrypi:8080/" "$CONF_DEST"; then
    echo "=> $SCRIPT_NAME: repo entry installed at $CONF_DEST"
else
    echo "=> $SCRIPT_NAME: ERROR: $CONF_DEST does not contain the Pi repo entry"
    exit 1
fi

echo "=> $SCRIPT_NAME: done. flowerpower: pacman -Sy && pacman -S hylki"
