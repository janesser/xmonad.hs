#!/usr/bin/fish

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash
bass source $NVM_DIR/nvm.sh ';' nvm install --lts --latest-npm

fisher install jorgebucaran/nvm.fish
set --global nvm_data $NVM_DIR

nvm use lts
npm i -g pnpm