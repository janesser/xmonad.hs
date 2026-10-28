#!/bin/bash

# SAFEGUARD: snap-only script. `snap` is an apt-flavoured package manager — on a
# pacman/yay box it does not exist, so skip silently instead of failing the whole
# `cz apply`.
command -v snap >/dev/null || exit 0

LANG=C
snap list --all | awk '/disabled/{print $1, $3}' |
    while read snapname revision; do
        sudo snap remove "$snapname" --revision="$revision"
    done
