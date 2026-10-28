#!/bin/bash

# SAFEGUARD: apt-only script. On a pacman/yay box (e.g. omarchy) `apt` does not
# exist — skip silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

sudo apt clean

## CLEAN UPS

# TeamViewer was removed from the repo (etc/apt sources + keyring). This
# idempotent script purges any leftover TeamViewer install on every apply.
bash "$CHEZMOI_SOURCE_DIR/uninstaller/teamviewer_uninstall.sh"

# The Olla system unit (olla.service) is removed from the repo; this
# idempotent script purges any leftover Olla install on every apply.
bash "$CHEZMOI_SOURCE_DIR/uninstaller/olla_service_uninstall.sh"

sudo apt remove --purge -y unattended-upgrades
sudo apt remove --purge -y tmux
sudo apt remove --purge -y postfix
sudo apt remove --purge -y pavucontrol

# sudo apt -o Dpkg::Options::=--force-confmiss reinstall pi-greeter

if command -v raspi-config >/dev/null
then
    sudo apt remove --purge -y rpd-x-all
    sudo apt remove --purge -y rpd-x-extras
else
    echo No uninstalls are defined.
fi
