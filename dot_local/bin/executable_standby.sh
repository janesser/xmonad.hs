#!/bin/bash

TARGETS="sleep.target suspend.target hibernate.target hybrid-sleep.target"
MASK_PROBE="sleep.target"
MASKED=""

if systemctl list-unit-files --state=masked|grep $MASK_PROBE; then
    MASKED=true
else
    MASKED=false
fi

do_toggle() {
    if [ "$MASKED" ]; then
        sudo systemctl unmask $TARGETS
    else
        sudo systemctl mask $TARGETS
    fi
}

case "${1:-status}" in
    toggle)
        do_toggle
        ;;
    -b)
        if [ $MASKED ]; then echo P; fi
        ;;
    status)
        echo "standby: $(MASKED)"
        ;;
esac
