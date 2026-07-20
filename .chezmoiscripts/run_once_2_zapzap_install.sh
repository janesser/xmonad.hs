#!/bin/bash

ZAPZAP_VERSION=6.5.2.5

uv tool install poetry
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
    uv build
    uv tool install dist/zapzap-$ZAPZAP_VERSION-*.whl
    # stop running instances
    pkill -f "tools/zapzap"
    # x-whatsapp &
else
    exit 1
fi
