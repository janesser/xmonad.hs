#!/bin/bash

TARGETS="sleep.target suspend.target hibernate.target hybrid-sleep.target"
MASK_PROBE="sleep.target"
MASKED=""

if [ "$(readlink /etc/systemd/system/sleep.target)" = "/dev/null" ]; then
    MASKED=true
else
    MASKED=false
fi

do_toggle() {
    if [ "$MASKED" = true ]; then
        sudo systemctl unmask $TARGETS
    else
        sudo systemctl mask $TARGETS
    fi
}

case "${1:-status}" in
    on)
        MASKED=true
        do_toggle
        ;;
    off)
        MASKED=false
        do_toggle
        ;;
    -t|toggle)
        do_toggle
        ;;
    -b|bar)
        if [ $MASKED = true ]; then echo P; fi
        ;;
    *|-s|status)
        echo "standby: $MASKED"
        ;;
esac
