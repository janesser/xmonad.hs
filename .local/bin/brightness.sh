#!/bin/bash
OP=$1
ACT=`xfpm-power-backlight-helper --get-brightness`
MAX=`xfpm-power-backlight-helper --get-max-brightness`

if [ -z $OP ] ; then
    printf '%3.0f%%' $(($ACT * 100 / $MAX))
else
    NEW=$(($ACT $OP 8))
    if [ "$NEW" -gt "$MAX" ] ; then
        NEW=$MAX
    fi
    pkexec  xfpm-power-backlight-helper --set-brightness $NEW
fi