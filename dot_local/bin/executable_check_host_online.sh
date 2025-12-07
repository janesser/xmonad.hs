#!/bin/bash
## in fish-shell start with 'check_host_online.sh &;disown'

HOST=$1
LAST_SEEN_ONLINE=never
CHECK_INTERVAL=${CHECK_INTERVAL:=5}

if [[ -e ~/.home_gateway_mac ]]
then
    HOME_GATEWAY_MAC=`cat ~/.home_gateway_mac`
    DEFAULT_GATEWAY_MAC=`arp -a|grep -e '^_gateway '|awk '{print $4;}'`
    if [[ "$DEFAULT_GATEWAY_MAC" = "$HOME_GATEWAY_MAC" ]]
    then
        echo We are home.
    else
        exit 1
    fi
else
    echo "Use 'set_home_gateway_mac.sh' when at home."
    exit 0
fi

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
