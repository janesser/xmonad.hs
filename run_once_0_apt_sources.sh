#!/bin/bash

# charm glow
# element.io
# nvidia-container-toolkit
# signal
# teamviewer

if [ $CHEZMOI_ARCH = "amd64" ]; then
    cd $CHEZMOI_SOURCE_DIR
    sudo cp ./etc/apt/keyrings/* /etc/apt/keyrings/
    sudo cp ./etc/apt/sources.list.d/* /etc/apt/sources.list.d/
else
    echo $CHEZMOI_ARCH not supported for additional apt sources.
fi

sudo apt install extrepo

# TODO find signal for arm64, e.g. https://github.com/dennisameling/Signal-Desktop
