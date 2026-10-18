#!/bin/bash

GHCUP_ON_PATH=`which ghcup`
if [ -n "$GHCUP_ON_PATH" ]; then
    echo ghcup already installed.
    exit 0;
fi

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
