#!/bin/bash

# Helix is installed via the official Snap package (classic).
# The old ppa:maveonair/helix-editor is unmaintained and only ships noble (24.04)
# packages, so it is broken on Ubuntu 24.10+ / 26.04 (missing release file).
# Upstream snap docs: https://docs.helix-editor.com/package-managers.html#snap
# Remove the unmaintained PPA source file(s) if a previous install added them
# (suite-agnostic glob; idempotent, no apt update triggered). `rm` is within
# the CHEZMOI_PKGS sudoers alias.
sudo rm -f /etc/apt/sources.list.d/maveonair-ubuntu-helix-editor*.sources \
           /etc/apt/sources.list.d/maveonair-ubuntu-helix-editor*.list 2>/dev/null || true
if snap list helix >/dev/null 2>&1; then
  echo "helix already installed via snap ($(snap info helix --json 2>/dev/null | tr -d '\n' | grep -o '"Installed":\"[^\"]*\"' | head -1)); skipping"
else
  sudo snap install --classic helix
  # Remove the old, unmaintained PPA apt package if it was installed previously.
  sudo apt remove -y helix
  sudo autoremove -y
fi

# Language Servers, see https://docs.helix-editor.com/lang-support.html
## TODO work through https://medium.com/@CaffeineForCode/helix-setup-for-markdown-b29d9891a812
sudo snap install bash-language-server --classic
sudo snap install marksman
cargo uninstall markdown-oxide 2>/dev/null || true # cleanup earlier install

# setting git core.editor
git config --global core.editor hx
