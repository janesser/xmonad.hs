#!/bin/bash

sudo apt install nala
#sudo nala fetch
sudo nala update

NALA_VERSION=`nala --version`

if [[ $NALA_VERSION == "nala 0.15.1" ]]; then
    nala --install-completion=fish
else
    nala --install-completion
fi