#!/bin/bash
OP=$1
ACT=`xfpm-power-backlight-helper --get-brightness`
MAX=`xfpm-power-backlight-helper --get-max-brightness`
pkexec  xfpm-power-backlight-helper --set-brightness $((($ACT $OP 10)%($MAX+1)))