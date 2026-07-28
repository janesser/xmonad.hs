#/bin/bash

uv tool install ratarmount
mkdir -p ./linux-source-6.8.0
sudo apt install -y linux-source-6.8.0

ratarmount -o modules=subdir,subdir=linux-source-6.8.0 /usr/src/linux-source-6.8.0.tar.bz2 ./linux-source-6.8.0/
