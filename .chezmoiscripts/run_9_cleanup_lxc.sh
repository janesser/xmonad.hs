#!/bin/bash

if [[ -n "$(lxc project list -f compact | grep snapcraft)" ]]; then
    lxc project switch snapcraft
    lxc list -f compact | awk '{print $1;}' | tail -n+2 | xargs -r lxc delete
fi