#!/bin/bash

# SAFEGUARD: apt-only script. `apt` sources live under /etc/apt — on a pacman/yay
# box that directory does not exist, so skip silently instead of failing the whole
# `cz apply`.
command -v apt >/dev/null || exit 0

# nothing to clean up: all boxes are already clean
if snap list helix >/dev/null 2>&1; then
  echo "helix already installed via snap ($(snap info helix --json 2>/dev/null | tr -d '\n' | grep -o '"Installed":\"[^\"]*\"' | head -1)); skipping"
else
  sudo snap install --classic helix
  # Remove the old, unmaintained PPA apt package if it was installed previously.
  sudo apt remove -y helix
  sudo autoremove -y
fi

# setting git core.editor
git config --global core.editor hx
