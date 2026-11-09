#!/bin/bash

set -uo pipefail

command -v omarchy >/dev/null || exit 0

omarchy pkg add \
    signal-desktop \
    nextcloud-client \
    keepassxc \
    helix \
    rustdesk \
    wakeonlan \
    lazygit \
    librewolf

omarchy plugin add https://github.com/jesusarchive/omarchy-dynamic-menu.git --enable --yes
omarchy plugin add https://github.com/TheMogli/ToDoma.git --enable --yes


yay -Syy
yay -S --noconfirm \
    cnrdrvcups-lb-bin \
    pwvucontrol \
    rptui-bin



if ! grep raspberrypi /etc/pacman.conf; then
    sudo install etc/pacman.conf /etc/pacman.conf
else
    echo lan repo already configured.
fi

sudo pacman -Syy
sudo pacman -S --noconfirm \
    hylki-bin \
    rencal-bin \
    vscodium-bin

exit 0
