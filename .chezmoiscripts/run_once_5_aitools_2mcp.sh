#!/bin/bash

# UNUSED

# lspci needs no sudo and detects the GPU even with no driver loaded, the
# right presence check here (lsmod would false-negative if the module isn't
# loaded yet, e.g. on a fresh setup).
if ! lspci 2>/dev/null | grep -iq nvidia; then
    echo "$(basename $0): No NVIDIA GPU detected, skipping..."
    exit 0
fi

sudo snap install task --classic

cd ~/projs
git clone https://github.com/stacklok/toolhive.git
cd toolhive

task install