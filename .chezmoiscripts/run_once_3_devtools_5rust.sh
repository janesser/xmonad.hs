#!/bin/bash

sudo apt install -y rustup
rustup default stable

sudo snap remove rustup
exit 0
