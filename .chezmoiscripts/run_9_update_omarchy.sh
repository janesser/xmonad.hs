#!/bin/bash

# SAFEGUARD: `omarchy` only exists on pacman/yay boxes — skip silently on apt/nala/snap
# boxes instead of failing the whole `cz apply`.
command -v omarchy >/dev/null || exit 0

omarchy update -y # FIXME bad sudo usage for now
