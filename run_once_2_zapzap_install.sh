#!/bin/bash

ZAPZAP_VERSION=6.3.3

sudo apt install -y python-dbus-dev libglib2.0-dev pyqt6-dev-tools

mkdir -p ~/projs; cd ~/projs
if [[ -d "zapzap" ]]; then
    echo zapzap is already checked out.
else
    git clone --depth 1 https://github.com/rafatosta/zapzap.git --single-branch  --branch=$ZAPZAP_VERSION
fi

if cd ~/projs/zapzap
then
    git fetch
    git reset --hard $ZAPZAP_VERSION
    echo resetting present env.
    poetry self add "poetry-dynamic-versioning[plugin]"
    poetry dynamic-versioning enable
    poetry env remove --all
    poetry lock
    poetry install
    poetry add dbus-python
else
    exit 1
fi