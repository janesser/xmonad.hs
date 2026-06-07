#!/bin/bash

if source ~/.nvm/nvm.sh
then
    nvm install lts/krypton --lts --latest-npm --default
    nvm install-latest-npm
    
    # https://helloacm.com/how-to-clean-up-nvm-node-versions-except-one/
    KEEP_VERSION=`nvm ls-remote --lts --no-colors | tail -n 1 | grep -oE "v[0-9\.]+"`
    echo Keeping version $KEEP_VERSION.
    # FIXED nvm ls in bash shows less versions than fisher plugin
    ## check $NVM_DIR what native nvm uses ~/.nvm
    ## check $nvm_data what nvm.fish uses ~/.local/share/nvm
    for v in $(nvm ls --no-colors --no-alias | \
            grep -v $KEEP_VERSION | \
            grep -v system | \
            grep -oE "v[0-9\.]+"); do
        echo Uninstalling $v.
        nvm uninstall $v
    done
else
    echo ~/.nvm/nvm.sh not available \(yet\).
fi