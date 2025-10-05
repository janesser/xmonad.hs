#!/usr/bin/fish

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

sudo apt install fzf fdclone
fisher install PatrickF1/fzf.fish
