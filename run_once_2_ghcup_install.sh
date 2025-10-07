#!/bin/bash

GHCUP_ON_PATH=`which ghcup`
if [ -n "$GHCUP_ON_PATH" ]; then
    echo ghcup already installed.
    exit 0;
fi

sudo apt install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 pkg-config
# answer N to fish.config PATH prepending
# answer Y to haskell-language-server
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh