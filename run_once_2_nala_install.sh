#!/bin/bash

sudo apt install nala
#sudo nala fetch
sudo nala update

nala --install-completion bash

if [ -x $(command -v fish) ]; then
    fish -c "nala --install-completion fish"
fi
