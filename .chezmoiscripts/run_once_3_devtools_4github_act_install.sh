#!/bin/bash

sudo apt install -y podman-docker golang-go
go install github.com/nektos/act@latest

# fix permission trouble
## /var/run/docker.socket points where no one may reach

sudo groupadd docker # might not exist

sudo chown root:docker /run/podman
sudo chmod 750 /run/podman # 700 before
# glob expands in this shell before sudo sees it, so no wildcard reaches sudo;
# chmod/chown with any args are already authorized in the CHEZMOI_PKGS alias.
sudo chown root:docker /run/podman/*.sock
sudo chmod 660 /run/podman/*.sock # 600 before

sudo usermod -a -G docker $USER

export PATH=~/go/bin:$PATH
act --version