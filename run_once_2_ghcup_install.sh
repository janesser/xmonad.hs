#!/bin/bash

GHCUP_ON_PATH=`which ghcup`
if [ -n $GHCUP_ON_PATH ]; then
    echo ghcup already installed.
    exit 0;
fi

sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
# answer Y to haskell-language-server
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh