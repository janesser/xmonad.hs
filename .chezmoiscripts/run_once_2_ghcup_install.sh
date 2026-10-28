#!/bin/bash

GHCUP_ON_PATH=`which ghcup`
if [ -n "$GHCUP_ON_PATH" ]; then
    echo ghcup already installed.
    exit 0;
fi

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 pkg-config

mise install ghcup
mise use ghcup@latest
eval "$(mise activate bash)"

# TODO make proper update cycle
ghcup install ghc@recommended
ghcup install --set ghc recommended
ghcup install --set cabal recommended
ghcup install --set hls recommended
ghcup install --set stack recommended
ghcup gc --unset
