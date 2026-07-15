#!/bin/sh
lxc project switch snapcraft

lxc list -f compact | awk '{print $1;}' | tail -n+2 | xargs -r lxc delete