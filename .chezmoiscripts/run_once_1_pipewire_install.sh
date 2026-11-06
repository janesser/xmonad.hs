#!/bin/bash

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

if [ "$CHEZMOI_ARCH" = "amd64" ]; then
    # `install -d` replaces the old `mkdir -p`, so the dir is created with the
    # ownership + mode set atomically (it stays root:root 0755 either way).
    sudo install -d -o root -g root -m 0755 /etc/pipewire
    # `install -o/-g/-m` replaces the old `cp`: one atomic call, no chmod/chown.
    sudo install -o root -g root -m 0644 /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systemctl restart --user pipewire
else
    echo $CHEZMOI_ARCH not supported for pipewire setup.
fi
