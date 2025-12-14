#!/bin/bash

sudo apt install -y python3-pyqt6 python3-poetry
mkdir -p ~/projs; cd ~/projs

if cd ~/projs/zapzap
then
    echo resetting present env.
    poetry env remove --all
fi
