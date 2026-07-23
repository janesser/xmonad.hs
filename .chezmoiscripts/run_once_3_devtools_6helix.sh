#!/bin/bash

sudo add-apt-repository -y ppa:maveonair/helix-editor
sudo apt update
sudo apt install -y helix

# Language Servers, see https://docs.helix-editor.com/lang-support.html
## TODO work through https://medium.com/@CaffeineForCode/helix-setup-for-markdown-b29d9891a812
cargo install --locked --git https://github.com/Feel-ix-343/markdown-oxide.git
sudo snap install bash-language-server --classic

# setting git core.editor
git config --global core.editor hx