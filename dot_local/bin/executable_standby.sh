#!/bin/bash

OP=$1
TARGETS="sleep.target suspend.target hibernate.target hybrid-sleep.target"

if [ -z $OP ]; then
    OP="is-masked"
    sudo systemctl list-unit-files $TARGETS
elif [ "$OP" = "help" ]; then
    echo "$0 help|mask|unmask|is-masked"
else
    sudo systemctl $OP $TARGETS
fi
