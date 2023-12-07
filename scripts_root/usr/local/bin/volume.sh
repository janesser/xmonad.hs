#!/bin/bash
OP=$1
SINK=`pactl get-default-sink`
ACT=`pactl get-sink-volume $SINK | grep -Po '(?<=left..)[0-9]+'` 
pactl set-sink-volume $SINK $((($ACT $OP 2048) % 65536))