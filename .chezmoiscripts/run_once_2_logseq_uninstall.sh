#!/bin/bash

if command -v flatpak; then
    echo Uninstalling Logseq and flatpak

    flatpak uninstall -y --delete-data com.logseq.Logseq
    flatpak uninstall --unused

    sudo apt remove --purge -y flatpak
else
    echo flatpak already uninstalled !
fi

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0
