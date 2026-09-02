#!/bin/bash

asdf plugin update --all

for c in $(asdf list | grep "^\w"); do
    echo Updating $c
    asdf install $c latest
done

# TODO nodejs lts, java and gradle require fixing
