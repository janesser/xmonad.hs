#!/bin/bash

rm -fR ~/.nvm

asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git

asdf cmd nodejs update-nodebuild
LTS_VERSION=`asdf cmd nodejs resolve lts`

asdf install nodejs "$LTS_VERSION"
asdf set -u nodejs "$LTS_VERSION"