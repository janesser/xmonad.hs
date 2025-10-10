#!/bin/bash

# sudo apt -o Dpkg::Options::=--force-confmiss reinstall pi-greeter

if [ "$CHEZMOI_ARCH" = "arm64" ]; then
    sudo apt remove --purge rpd-x-all
    sudo apt remove --purge rpd-x-extras
else
    echo For $CHEZMOI_ARCH no uninstalls are defined.
fi