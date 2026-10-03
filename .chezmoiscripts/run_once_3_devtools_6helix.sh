#!/bin/bash

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

go install charm.land/glow/v3@latest
