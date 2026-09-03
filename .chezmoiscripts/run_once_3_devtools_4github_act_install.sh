#!/bin/bash

sudo apt install -y podman-docker golang-go
go install github.com/nektos/act@latest

# fix permission trouble
## /var/run/docker.socket points where no one may reach

sudo groupadd docker # might not exist

sudo chown root:docker /run/podman
sudo chmod 750 /run/podman # 700 before
sudo bash -c "chown root:docker /run/podman/*.sock"
sudo bash -c "chmod 660 /run/podman/*.sock" # 600 before

sudo usermod -a -G docker $USER

export PATH=~/go/bin:$PATH
act --version