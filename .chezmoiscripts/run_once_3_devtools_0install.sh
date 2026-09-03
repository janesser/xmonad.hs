#!/bin/bash

sudo apt install -y git gitk git-lfs direnv

# snap
sudo apt install -y snap
sudo snap install lxd

sudo snap remove --purge kubectl
sudo snap remove --purge helm

# git credential manager
## https://docs.github.com/en/get-started/git-basics/why-is-git-always-asking-for-my-credentials

curl -s https://api.github.com/repos/git-ecosystem/git-credential-manager/releases/latest| jq '.assets.[].browser_download_url | match(".*x64.*deb")|.string'| xargs curl -o /tmp/gcm.deb -L
sudo dpkg -i /tmp/gcm.deb
sudo apt --fix-broken -y install
git config --global credential.credentialStore secretservice
