#!/bin/bash

ZAPZAP_VERSION=6.2.3.1

# FIXME on ubuntu noble arm64: ImportError: libwebp.so.6: cannot open shared object file: No such file or directory
sudo apt install -y python-dbus-dev libglib2.0-dev

mkdir -p ~/projs; cd ~/projs
if [[ -d "zapzap" ]]; then
    echo zapzap is already checked out.
else
    git clone --depth 1 https://github.com/rafatosta/zapzap.git --single-branch  --branch=$ZAPZAP_VERSION
fi

if cd ~/projs/zapzap
then
    git reset --hard $ZAPZAP_VERSION
    echo resetting present env.
    poetry env remove --all
    poetry install
    poetry add dbus-python
else
    exit 1
fi