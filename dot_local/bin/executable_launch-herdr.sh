#!/bin/bash
# launch-herdr.sh — start herdr, asserting SSH keys are loaded.
#
# Assures that the system ssh-agent (systemd user service
# ~/.config/systemd/user/ssh-agent.service) has identities before launching
# herdr. Without them herdr cannot authenticate to its remote hosts and fails
# silently. If no key is loaded, `ssh-add` runs in a foot window so a key can be
# loaded interactively (the TTY it needs for the passphrase prompt).

set -euo pipefail

# 1. Assert: ssh-agent.service is active.
if ! systemctl --user is-active ssh-agent >/dev/null 2>&1; then
    echo "launch-herdr: ssh-agent.service is not active" >&2
    exit 1
fi

export SSH_AUTH_SOCK=/run/user/1000/ssh-agent.socket

# 2. Assert: keys loaded — `ssh-add` in a terminal if the agent has none.
if ! ssh-add -l >/dev/null 2>&1; then
    echo "launch-herdr: no SSH keys loaded, loading one with ssh-add" >&2
    foot -e ssh-add >/dev/null 2>&1 || true
fi

# Re-check after the prompt window closes.
if ! ssh-add -l >/dev/null 2>&1; then
    echo "launch-herdr: no SSH keys loaded, cannot manage remote hosts" >&2
    exit 1
fi

# 3. Keys are present; start herdr, letting it take over SSH auth.
exec foot herdr
