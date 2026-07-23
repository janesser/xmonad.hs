#!/bin/bash

sudo apt install -y git gitk git-lfs direnv

if [ "$CHEZMOI_ARCH" = "amd64" ]; then
    sudo apt install -y snap
    sudo snap install kubectl --classic
    sudo snap install helm --classic
else
    echo $CHEZMOI_ARCH not supported for snap.
fi
