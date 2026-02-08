#!/bin/bash

if source ~/.nvm/nvm.sh
then
    nvm install lts/krypton --lts --latest-npm --default
    nvm install-latest-npm
    
    # https://helloacm.com/how-to-clean-up-nvm-node-versions-except-one/
    KEEP_VERSION=`nvm ls-remote --lts --no-colors | tail -n 1 | awk '{print $2;}'`
    for version in $(nvm ls --no-colors --no-alias | \
            grep -v $KEEP_VERSION | \
            grep -v system ); do
        nvm uninstall $version
    done
else
    echo ~/.nvm/nvm.sh not available \(yet\).
fi