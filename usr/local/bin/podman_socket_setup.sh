#!/bin/bash
# /usr/local/bin/podman_socket_setup.sh  (installed to the target location by
# chezmoi)
#
# Root-owned wrapper. Set ownership + mode on the podman socket directory and its
# sockets so the docker group can use rootful podman without running as root.
# Called from run_once_3_devtools_3podman.sh via `sudo <this>`.
#
# NOT granted in sudoers: this script itself is the constraint — no chmod/chown
# of arbitrary files is available outside it.

chown root:docker /run/podman && chmod 750 /run/podman
for f in /run/podman/*.sock; do chown root:docker "$f" && chmod 660 "$f"; done

echo "ok: podman socket ownership + mode set for group docker"
