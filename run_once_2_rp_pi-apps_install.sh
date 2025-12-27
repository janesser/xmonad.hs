#!/bin/bash

if [ "$CHEZMOI_ARCH" = "arm64" ]; then
    wget -qO- https://raw.githubusercontent.com/Botspot/pi-apps/master/install | bash

    ~/pi-apps/manage install Signal
fi