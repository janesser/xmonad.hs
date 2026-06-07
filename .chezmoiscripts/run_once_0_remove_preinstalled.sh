#!/bin/bash

# sudo apt -o Dpkg::Options::=--force-confmiss reinstall pi-greeter

if command -v raspi-config >/dev/null
then
    sudo apt remove --purge -y rpd-x-all
    sudo apt remove --purge -y rpd-x-extras
else
    echo No uninstalls are defined.
fi