#!/bin/bash

# SAFEGUARD: rustup-only script. `rustup` is only installed on apt boxes
# (run_once_3_devtools_5rust.sh), so skip silently instead of failing the whole
# `cz apply` on a pacman/yay box.
command -v rustup >/dev/null || exit 0

rustup update

rustup toolchain install stable
