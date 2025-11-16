#!/bin/bash

## start with 'check_host_online.sh &;disown'

HOST=$1
LAST_SEEN_ONLINE=never

while true; do
    if ping -q -c 2 $HOST >/dev/null
    then
        #echo $HOST responds to ping.
        LAST_SEEN_ONLINE=`date`
    else
        notify-send "$HOST offline" "$HOST offline, last seen online $LAST_SEEN_ONLINE."
    fi
    sleep 2
done
