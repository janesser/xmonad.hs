#!/bin/bash

# SAFEGUARD: `omarchy` only exists on pacman/yay boxes — skip silently on apt/nala/snap
# boxes instead of failing the whole `cz apply`.
command -v omarchy >/dev/null || exit 0

# Run through the scoped passwordless-sudo drop-in (`/etc/sudoers.d/chezmoi-omarchy`,
# `(ALL) NOPASSWD: /usr/bin/omarchy`) so `cz update` needs no TTY password prompt.
# Same pattern as `run_9_update_nala.sh`.
sudo omarchy update -y
