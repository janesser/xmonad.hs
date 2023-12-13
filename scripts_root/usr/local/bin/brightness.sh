#!/bin/bash
OP=$1
ACT=`xfpm-power-backlight-helper --get-brightness`
MAX=`xfpm-power-backlight-helper --get-max-brightness`

if [ -z $OP ] ; then
    echo $ACT
else
    NEW=$(($ACT $OP 10))
    if [$NEW > $MAX] ; then
        NEW=$MAX
    fi
    pkexec  xfpm-power-backlight-helper --set-brightness $NEW
fi