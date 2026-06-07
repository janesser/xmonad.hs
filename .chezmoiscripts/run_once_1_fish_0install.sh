#!/bin/bash

sudo add-apt-repository ppa:fish-shell/release-4
sudo apt update
sudo apt install -y fish
chsh -s $(which fish)
