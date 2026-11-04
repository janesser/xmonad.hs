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
#   AUR (yay):     vscodium-bin pwvucontrol rencal-bin rptui-bin
#
#   hylki-debug and libgnome-keyring/libgnome-keyring-debug come in as
#   dependencies of packages that pacman installs from the lan_repo.
#
# NOTE: hylki comes from the Raspberry Pi repo server now ([repo] in
# /etc/pacman.d/conf.d/lan_repo.conf, Server = http://raspberrypi:8080/) — see
# run_once_arch_repo_pacman_conf.sh. Only set up the repo server first,
# then the next cz update will install hylki from there.
#
# The AUR installs below could also be built for the lan_repo instead (see
# executable_build-lan-repo.sh --package), which would let pacman install them
# from the same repo as hylki.
#
# SAFEGUARD: `pacman` and `yay` do not exist on apt boxes, so skip silently
# instead of failing the whole `cz apply`.

set -uo pipefail

# --- guards: pacman-based boxes only ---
command -v pacman >/dev/null || exit 0
command -v yay >/dev/null || exit 0

if ! grep raspberrypi /etc/pacman.conf; then
    sudo install etc/pacman.conf /etc/pacman.conf
else
    echo lan repo already configured.
fi

echo "==> pacman: installing official repo packages"
sudo pacman -S --noconfirm --needed \
    signal-desktop \
    nextcloud-client \
    keepassxc \
    helix \
    rustdesk \
    wakeonlan

echo "==> yay: installing AUR packages"
yay -S --noconfirm --needed \
    vscodium-bin \
    pwvucontrol \
    rencal-bin \
    rptui-bin

# `omarchy plugin add` refuses to continue without a confirmation — `--yes`
# makes this run silently inside `cz apply` (no TTY).
omarchy plugin add https://github.com/jesusarchive/omarchy-dynamic-menu.git --enable --yes
omarchy plugin add https://github.com/TheMogli/ToDoma.git --enable --yes

# `pacman -S` prompts on reinstalls from another repo, so `--noconfirm` is
# required: these scripts run without a TTY and never get an answer.
sudo pacman -S --noconfirm hylki-bin

exit 0
