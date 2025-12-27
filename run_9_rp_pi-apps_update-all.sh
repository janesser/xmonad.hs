#!/bin/bash

if [ "$CHEZMOI_ARCH" = "arm64" ]; then
    ~/pi-apps/manage update-all
fi