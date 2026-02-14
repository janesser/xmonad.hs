#!/bin/bash

curl -fsSL https://apt.fury.io/nushell/gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/fury-nushell.gpg
echo "deb [signed-by=/etc/apt/keyrings/fury-nushell.gpg] https://apt.fury.io/nushell/ /" | sudo tee /etc/apt/sources.list.d/fury-nushell.list
sudo apt update
sudo apt install -y nushell

# https://github.com/nushell/awesome-nu
# https://github.com/bevry/dorothy/ <- try this soon