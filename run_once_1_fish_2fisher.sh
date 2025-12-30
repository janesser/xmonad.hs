#!/usr/bin/fish

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

fisher install oh-my-fish/plugin-foreign-env

sudo apt install -y fzf fdclone
fisher install PatrickF1/fzf.fish

fisher install halostatue/fish-chezmoi@v1

fisher remove budimanjojo/tmux.fish
exit 0