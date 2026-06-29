#!/bin/bash

lsmod | grep nvidia
if [[ $? -ne 0 ]]; then
    echo "$(basename $0): No nvidia module loaded in kernel, skipping..."
    exit 0
fi

sudo snap install task --classic

cd ~/projs
git clone https://github.com/stacklok/toolhive.git
cd toolhive

task install