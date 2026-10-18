#!/bin/bash

for f in $(find ~/.config/evolution/sources); do
    if [[ -f $f ]] && [[ $f != *.lastnotified ]] && [[ -n $(grep LastNotified $f) ]]; then
        grep LastNotified $f | tail -n 1 > $f.lastnotified # latest occurrence wins
        echo written to $f.lastnotified
        sed -i '/^LastNotified/d' $f
        echo removed LastNotified from $f
    fi
done
