#!/bin/bash

sudo apt install git gitk git-lfs

sudo apt install snap
sudo snap install kubectl --classic
sudo snap install helm --classic

# TODO https://sdkman.io/install/
# sdkman is stronly bash based and requires a wrapper like this: https://github.com/reitzig/sdkman-for-fish
# curl -s "https://get.sdkman.io" | bash
