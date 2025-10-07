#!/bin/bash

sudo apt install git gitk git-lfs

if [ $CHEZMOI_ARCH = "amd64" ]; then
    sudo apt install snap
    sudo snap install kubectl --classic
    sudo snap install helm --classic
else
    echo $CHEZMOI_ARCH not supported for snap.
fi
