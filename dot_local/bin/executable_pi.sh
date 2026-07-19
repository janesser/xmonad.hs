#!/bin/bash

# activate node context
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
nvm use --lts

# Update pi before launching
pi update --all
chezmoi re-add ~/.pi/agent/settings.json

# Start pi
pi "$@"
