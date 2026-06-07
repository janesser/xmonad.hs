#!/bin/bash

for f in $(find ~/.config/evolution/sources); do
    if [[ -f $f ]] && [[ $f == *.lastnotified ]]; then
        orig=$(basename $f .lastnotified)
        if [[ -n $(grep LastNotified $orig) ]]; then
            echo LastNotified already patched
        else
            cat $f | tee -ap ~/.config/evolution/sources/$orig
            echo writing to $orig $(cat $f)
        fi
        rm -f $f
    fi
done
