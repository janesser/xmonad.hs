#!/bin/bash

set -euo pipefail

say() { printf "%b%s%b\n" "$(printf '\033[0;34m')" "$1" "$(printf '\033[0m')"; }

sudo rm -f /etc/sudoers.d/chezmoi-pi*

say "OK: /etc/sudoers.d/chezmoi-pi* removed."
