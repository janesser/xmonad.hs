#!/bin/bash
# run_once_9_cleanup_snap_install.sh
#
# One-time migration: installs snapd and the snap builds of Signal and Element.
# Runs once (run_once) during `cz apply`, AFTER run_once_9_cleanup_remove_
# legacy.sh (which purges the apt builds of signal-desktop / element-desktop)
# — see that script for the ordering rationale.
#
# Idempotent + guarded: snapd is only (re)installed when missing, and each snap
# is only installed when not already present, so a re-apply is cheap and an
# interrupted install can be safely retried. Sudo is used only for commands in
# the scoped NOPASSWD sudoers drop-in (apt, snap).

set -u

# --- snapd (prerequisite for `snap install`) -----------------------------
if command -v snap >/dev/null 2>&1; then
    echo "snapd already present — skipping 'apt install snap'"
else
    sudo apt install -y snap
fi

# --- snap builds (only install what is missing) --------------------------
for app in signal-desktop element-desktop; do
    if snap list "$app" >/dev/null 2>&1; then
        echo "$app already installed — skipping"
    else
        sudo snap install "$app"
    fi
done
