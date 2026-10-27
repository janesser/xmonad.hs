#!/bin/bash
#
# Install Signal via pi-apps, non-interactive.
#
# pi-apps' `manage install` is interactive by default: it uses `yad` GUI prompts
# (which fail silently without a DISPLAY) and sets `DEBIAN_FRONTEND=gnome`,
# which makes the app's own install scripts ask questions. `DEBIAN_FRONTEND=noninteractive`
# suppresses those; `yad` simply does nothing when there is no display.
#
# Non-fatal: always exits 0 so a failed install never breaks `cz apply`.

set -uo pipefail

if [ "$CHEZMOI_ARCH" != "arm64" ]; then
    echo "==> pi-apps: skipping (arm64 only, this is $CHEZMOI_ARCH)"
    exit 0
fi

if [ ! -x "$HOME/pi-apps/manage" ]; then
    echo "==> pi-apps: ~/pi-apps/manage not found; install run_once_2_rp_pi-apps_install.sh first"
    exit 0
fi

export DEBIAN_FRONTEND=noninteractive

wget -qO- https://raw.githubusercontent.com/Botspot/pi-apps/master/install | bash || {
    echo "==> pi-apps: install script failed (non-fatal)"
    exit 0
}

~/pi-apps/manage install Signal || {
    echo "==> pi-apps: install Signal failed (non-fatal)"
    exit 0
}

exit 0