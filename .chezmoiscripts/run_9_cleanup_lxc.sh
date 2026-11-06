#!/bin/bash

command -v lxc || exit 0

if [[ -n "$(lxc project list -f compact | grep snapcraft)" ]]; then
    lxc project switch snapcraft
    lxc list -f compact | awk '{print $1;}' | tail -n+2 | xargs -r lxc delete
fi