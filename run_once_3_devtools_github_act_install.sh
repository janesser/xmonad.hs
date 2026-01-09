#!/bin/bash

sudo apt install -y podman-docker golang-go
go install github.com/nektos/act@latest

# fix permission trouble
## /var/run/docker.socket points where no one may reach
sudo chown root:docker /run/podman
sudo chmod 750 /run/podman # 700 before
sudo chown root:docker /run/podman/podman.sock
sudo chmod 660 /run/podman/podman.socket # 600 before

act --version