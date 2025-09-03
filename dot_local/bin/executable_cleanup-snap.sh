#!/bin/bash

LANG=C

snap_cleanup() {
    snap list --all | awk '/disabled/{print $1, $3}' |
        while read snapname revision; do
            snap remove "$snapname" --revision="$revision"
        done
    echo all disabled revisions cleaned.
}

export -f snap_cleanup

su "$USER" -s /usr/bin/bash -c 'snap_cleanup'