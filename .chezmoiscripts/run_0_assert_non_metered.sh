#!/bin/bash

# Source - https://stackoverflow.com/a/43287215

METERED=`nmcli -t -f GENERAL.DEVICE,GENERAL.METERED dev show | grep METERED:yes`

if [[ -n "$METERED" ]]; then
    echo Connected over metered wire aborting update procedure.
    exit 1
fi
