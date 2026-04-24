#!/bin/bash

sudo add-apt-repository ppa:nextcloud-devs/client -y
sudo apt install -y nextcloud-desktop
mkdir -p ~/Nextcloud

## remove pcloud and koofr
rm -fR ~/.pcloud
rm ~/.local/bin/pcloud

rm -fR ~/.koofr
rm -fR ~/.koofr-dist