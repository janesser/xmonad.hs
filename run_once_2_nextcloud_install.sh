#!/bin/bash

sudo apt install -y nextcloud-desktop
mkdir -p ~/Nextcloud

## remove pcloud and koofr
rm -fR ~/.pcloud
rm ~/.local/bin/pcloud

rm -fR ~/.koofr
rm -fR ~/.koofr-dist