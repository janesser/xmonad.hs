#!/bin/bash

cd ~/projs
git clone https://github.com/lobehub/lobehub.git
cd lobehub

podman-compose -f docker-compose.development.yml build

# find ai runner behind 'host.containers.internal'
