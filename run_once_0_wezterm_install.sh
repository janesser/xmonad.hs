#!/bin/bash

curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /etc/apt/keyrings/wezterm-fury.gpg
sudo chmod 644 /etc/apt/keyrings/wezterm-fury.gpg
echo 'deb [signed-by=/etc/apt/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list
sudo apt update

sudo apt install -y wezterm

wezterm shell-completion --shell fish > ~/.config/fish/completions/wezterm.fish

sudo update-alternatives --set x-terminal-emulator /usr/bin/open-wezterm-here

## clean-up kitty if present
sudo update-alternatives --remove x-terminal-emulator /usr/local/bin/kitty
sudo rm -fR /usr/local/bin/kitty /usr/local/bin/kitten \
    /usr/local/lib/kitty /usr/share/doc/kitty \
    /usr/local/share/icons/hicolor/scalable/apps/kitty.svg \
    /usr/local/share/icons/hicolor/256x256/apps/kitty.png \
    /usr/local/share/terminfo/x/xterm-kitty \
    '/usr/local/share/applications/kitty*' \
    '/usr/local/share/man/man1/kitten*' \
    '/usr/local/share/man/man5/kitty*'
exit 0