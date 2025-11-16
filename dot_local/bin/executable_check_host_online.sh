#!/bin/bash

## in fish-shell start with 'check_host_online.sh &;disown'

HOST=$1
LAST_SEEN_ONLINE=never
CHECK_INTERVAL=${CHECK_INTERVAL:=5}

# fail fast if host can't be reached
if ping -q -c 2 $HOST >/dev/null
then
    LAST_SEEN_ONLINE=`date`
else
    exit 1
fi


while true; do
    if ping -q -c 2 "$HOST" >/dev/null
    then
        #echo $HOST responds to ping.
        LAST_SEEN_ONLINE=`date`
    else
        notify-send "$HOST offline" "$HOST offline, last seen online $LAST_SEEN_ONLINE."
    fi
    sleep "$CHECK_INTERVAL"
done
