#!/bin/bash

sudo apt install gvfs gvfs-fuse gvfs-backends

INSTALLER_DIR=/tmp/koofr-install/

mkdir -p $INSTALLER_DIR
pushd .
curl -L --output - https://app.koofr.net/dl/apps/linux64 | tar xzv -C $INSTALLER_DIR
cd "$INSTALLER_DIR/koofr"
. ./installer.sh
popd
rm -fR $INSTALLER_DIR