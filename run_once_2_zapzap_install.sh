#!/bin/bash

ZAPZAP_VERSION=6.2.3.1

sudo apt install -y python3-webview python3-pyqt6 python3-poetry

mkdir -p ~/projs; cd ~/projs
git clone --depth 1 https://github.com/rafatosta/zapzap.git --single-branch  --branch=$ZAPZAP_VERSION

if cd ~/projs/zapzap
then
    git reset --hard $ZAPZAP_VERSION
    echo resetting present env.
    poetry env remove --all
    poetry self sync
    poetry install
    poetry update --sync dbus-python
else
    exit 1
fi