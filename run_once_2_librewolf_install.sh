#!/bin/bash

sudo extrepo enable librewolf
sudo apt update
sudo apt install librewolf
sudo update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250
sudo update-alternatives --auto x-www-browser

xdg-settings set default-web-browser librewolf.desktop