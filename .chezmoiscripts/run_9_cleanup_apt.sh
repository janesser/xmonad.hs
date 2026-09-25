#!/bin/bash

sudo apt clean

## CLEAN UPS

# TeamViewer was removed from the repo (etc/apt sources + keyring). This
# idempotent script purges any leftover TeamViewer install on every apply.
bash "$CHEZMOI_SOURCE_DIR/uninstaller/teamviewer_uninstall.sh"

sudo apt remove --purge -y unattended-upgrades
sudo apt remove --purge -y tmux
sudo apt remove --purge -y postfix

# sudo apt -o Dpkg::Options::=--force-confmiss reinstall pi-greeter

if command -v raspi-config >/dev/null
then
    sudo apt remove --purge -y rpd-x-all
    sudo apt remove --purge -y rpd-x-extras
else
    echo No uninstalls are defined.
fi
