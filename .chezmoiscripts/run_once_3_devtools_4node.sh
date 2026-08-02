#!/usr/bin/fish

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash
bass source ~/.nvm/nvm.sh ';' nvm install --lts --latest-npm

fisher install jorgebucaran/nvm.fish

# SYMPTOM nvm ls in bash shows less versions than fisher plugin
## check $NVM_DIR what native nvm uses ~/.nvm
## check $nvm_data what nvm.fish uses ~/.local/share/nvm
## NVM_DIR is set in .profile, nvm-fish defaults apply from ~/.config/nvm.fish

# FIX align $nvm_data with $NVM_DIR
echo "set --global nvm_data \$NVM_DIR/versions/node" > ~/.config/fish/conf.d/0nvm.fish

set --global nvm_data ~/.nvm/versions/node # oneshot
#cp ~/.local/share/nvm/.index ~/.nvm/versions/node/

nvm ls-remote
nvm use lts
