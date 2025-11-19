#!/bin/bash

sudo apt install -y git gitk git-lfs

if [ "$CHEZMOI_ARCH" = "amd64" ]; then
    sudo apt install -y snap
    sudo snap install kubectl --classic
    sudo snap install helm --classic
    # install rust/cargo
    sudo snap install rustup --classic
    rustup default stable
else
    echo $CHEZMOI_ARCH not supported for snap.
fi
