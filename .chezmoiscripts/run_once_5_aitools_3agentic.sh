#!/bin/bash

sudo apt install -y fd-find
sudo apt remove --purge -y fdclone
## https://github.com/earendil-works/pi/issues/3882
ln -sf /usr/bin/fdfind ~/.pi/agent/bin/fd

mise install pi
mise use -g pi

pi install npm:pi-web-access
pi install npm:@hypabolic/crossbar

sudo snap install ghidra
pi install npm:pi-ghidra
