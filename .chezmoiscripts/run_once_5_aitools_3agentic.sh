#!/bin/bash

# delete whatever was installed before
## settings.json is chezmoi managed
rm -fR ~/.pi 

sudo apt install -y fd-find
sudo apt remove --purge -y fdclone
## https://github.com/earendil-works/pi/issues/3882
ln -sf /usr/bin/fdfind ~/.pi/agent/bin/fd

mise install pi
mise use -g pi

# by settings.json: pi install npm:pi-web-access
# by settings.json: pi install npm:@hypabolic/crossbar

sudo snap install ghidra
# by settings.json: pi install npm:pi-ghidra

pushd $CHEZMOI_SOURCE_DIR
npx bmad-method install --directory . --modules bmm --tools pi --yes

pi list
