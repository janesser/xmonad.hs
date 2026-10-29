#!/bin/bash

# SAFEGUARD: podman-only script. `podman` is only installed on apt boxes
# (run_once_3_devtools_3podman.sh), so skip silently instead of failing the
# whole `cz apply` on a pacman/yay box.
command -v podman >/dev/null || exit 0

# Rootful install (run_once_3_devtools_3podman.sh): /run/podman is root:docker
# and the socket is 0660, so pruning needs root.
sudo podman system prune -f
