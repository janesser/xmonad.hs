#!/bin/bash

sudo apt install -y nala
#sudo nala fetch
sudo nala update

nala --install-completion bash

if command -v fish > /dev/null &2>1
then
    fish -c "nala --install-completion fish"
fi
