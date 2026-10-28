#!/bin/bash

# SAFEGUARD: apt-only script. `add-apt-repository` and `apt` do not exist on a
# pacman/yay box, so skip silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo add-apt-repository ppa:nextcloud-devs/client -y
sudo apt install -y nextcloud-desktop
mkdir -p ~/Nextcloud

## remove pcloud and koofr
rm -fR ~/.pcloud
rm ~/.local/bin/pcloud

rm -fR ~/.koofr
rm -fR ~/.koofr-dist