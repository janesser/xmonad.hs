#!/bin/bash

cd ~/projs
git clone https://github.com/lobehub/lobehub.git
cd lobehub
git fetch
git checkout v2.1.20 # pin version

podman-compose -f ./docker-compose/dev/docker-compose.yml build

# find ai runner behind 'host.containers.internal'
