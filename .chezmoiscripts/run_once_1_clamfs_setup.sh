#!/bin/bash

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo apt install -y clamfs

# https://www.scribd.com/document/62059208/Clamfs-Module
if pushd ~/.downloads
then
    echo "clamfs ~/.downloads already prepared."
else
    mv ~/Downloads ~/.downloads
    mkdir ~/Downloads

    clamfs ~/.config/clamfs.d/downloads.xml
    echo clamfs activated.
fi
