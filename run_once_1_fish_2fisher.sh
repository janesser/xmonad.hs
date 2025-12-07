#!/usr/bin/fish

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

sudo apt install -y fzf fdclone
fisher install PatrickF1/fzf.fish

fisher install halostatue/fish-chezmoi@v1
