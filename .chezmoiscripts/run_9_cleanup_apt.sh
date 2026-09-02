#!/bin/bash

sudo apt clean

## CLEAN UPS

sudo apt remove --purge -y unattended-upgrades
sudo apt remove --purge -y tmux

# sudo apt -o Dpkg::Options::=--force-confmiss reinstall pi-greeter

if command -v raspi-config >/dev/null
then
    sudo apt remove --purge -y rpd-x-all
    sudo apt remove --purge -y rpd-x-extras
else
    echo No uninstalls are defined.
fi
