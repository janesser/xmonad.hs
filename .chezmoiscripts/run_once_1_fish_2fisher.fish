#!/usr/bin/fish

# SAFEGUARD: apt-only script. `apt` does not exist on a pacman/yay box, so skip
# silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

fisher install oh-my-fish/plugin-foreign-env

sudo apt install -y fzf fd-find
fisher install PatrickF1/fzf.fish

fisher install halostatue/fish-chezmoi@v1
fisher install halostatue/fish-direnv@v1
fisher install edc/bass

fisher remove budimanjojo/tmux.fish
fisher remove jorgebucaran/nvm.fish
fisher remove reitzig/sdkman-for-fish