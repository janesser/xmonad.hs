#!/bin/bash

# sudo apt -o Dpkg::Options::=--force-confmiss reinstall pi-greeter

if command -v raspi-config >/dev/null 2>&1
then
    sudo apt remove --purge -y rpd-x-all
    sudo apt remove --purge -y rpd-x-extras
else
    echo For $CHEZMOI_ARCH no uninstalls are defined.
fi