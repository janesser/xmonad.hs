#!/usr/bin/fish

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
source ~/.profile
nvm install lts

fisher install jorgebucaran/nvm.fish

# n8n
# npx -y n8n
