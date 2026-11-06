#!/bin/bash

# SAFEGUARD: apt-only script. The wezterm apt keyring lives under /etc/apt — on a
# pacman/yay box that directory does not exist, so skip silently instead of
# failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

# Install the wezterm GPG key into /etc/apt/keyrings as root:root 0644 — the mode
# apt requires for a signed-by keyring. `install` replaces the old `chmod 644`
# so ownership and mode are set atomically (no intermediate state to race).
# NOTE: piped streams can't go straight to `install`, so gpg dearmors to a temp
# file first; the destination is still owned by root from the start.
curl -fsSL https://apt.fury.io/wez/gpg.key | gpg --yes --dearmor > /tmp/wezterm-fury.gpg
sudo install -o root -g root -m 0644 /tmp/wezterm-fury.gpg /etc/apt/keyrings/wezterm-fury.gpg
echo 'deb [signed-by=/etc/apt/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list
sudo apt update

sudo apt install -y wezterm

wezterm shell-completion --shell fish > ~/.config/fish/completions/wezterm.fish

sudo update-alternatives --set x-terminal-emulator /usr/bin/open-wezterm-here

## experiment zellij
## zellij is managed by mise now (dot_config/mise/config.toml); no cargo build
export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)" || true
mise install zellij || true
sudo update-alternatives --set x-terminal-emulator $(which zutty)
# see .config/fish/conf.d/zellij.fish

## clean-up kitty if present
sudo update-alternatives --remove x-terminal-emulator /usr/local/bin/kitty \
    '/usr/local/share/man/man5/kitty*'
exit 0

## clean-up alacritty if present
sudo update-alternatives --remove x-terminal-emulator $HOME/.cargo/bin/alacritty
cargo uninstall alacritty 2>/dev/null || true
