#!/bin/bash
# run_once_2_pacman_yay_install.sh
#
# Install the desktop stack from pacman (official repos) and yay (AUR).
#
# This is the pacman/yay mirror of the apt scripts: it runs on pacman-based
# boxes (omarchy) and is skipped silently on apt-based boxes, so it is safe to
# run on every box in the fleet while `cz apply` updates them all.
#
# The package list is the state of the omarchy pilot box (flowerpower), where
# these are the packages that are *not* in the base ISO:
#   official repo: zen-browser-bin signal-desktop nextcloud-client keepassxc
#                  helix neovim rustdesk
#   AUR (yay):     vscodium-bin hylki hylki-debug libgnome-keyring
#                  libgnome-keyring-debug pwvucontrol rencal-bin rptui-bin
#
# SAFEGUARD: `pacman` and `yay` do not exist on apt boxes, so skip silently
# instead of failing the whole `cz apply`.

set -uo pipefail

# --- guards: pacman-based boxes only ---
command -v pacman >/dev/null || exit 0
command -v yay >/dev/null || exit 0

echo "==> pacman: installing official repo packages"
sudo pacman -S --noconfirm --needed \
    zen-browser-bin \
    signal-desktop \
    nextcloud-client \
    keepassxc \
    helix \
    neovim \
    rustdesk

echo "==> yay: installing AUR packages"
yay -S --noconfirm --needed \
    vscodium-bin \
    hylki \
    hylki-debug \
    libgnome-keyring \
    libgnome-keyring-debug \
    pwvucontrol \
    rencal-bin \
    rptui-bin

echo "==> pacman: done"
exit 0
