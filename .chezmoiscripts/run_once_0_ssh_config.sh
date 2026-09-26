#!/bin/bash
# run_once_0_ssh_config.sh
#
# Deploy the SSH client agent-forwarding drop-in to
# /etc/ssh/ssh_config.d/99-allow-agent-forward.conf.
#
# This is a run-ONCE script: chezmoi records that it has run and will not
# re-run it unless the script itself changes. The deploy is idempotent (cmp),
# so a re-run (e.g. after an edit) is safe.
#
# The drop-in lives in the ignored etc/ source tree (see
# etc/ssh/ssh_config.d/99-allow-agent-forward.conf). The client-side directive
# is ForwardAgent (see ssh_config(5)). NOTE:
#   AgentForwarding / AllowAgentForwarding are sshd_config (server-side) options
#   and are rejected by the ssh client ("Bad configuration option").
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in
# (AGENTS.md): mkdir, install.

set -euo pipefail

SRC_FILE="${CHEZMOI_SOURCE_DIR:-.}/etc/ssh/ssh_config.d/99-allow-agent-forward.conf"
DEST_DIR=/etc/ssh/ssh_config.d
DEST="${DEST_DIR}/99-allow-agent-forward.conf"

if [ ! -f "${SRC_FILE}" ]; then
    echo "⚠️  Drop-in source not found: ${SRC_FILE}"
    echo "   Create it (single directive: 'ForwardAgent yes'), then run cz apply again."
    exit 1
fi

# Idempotent backstop: already installed and unchanged -> nothing to do.
# Both source and installed file are world-readable, so `cmp` runs as jan.
if [ -f "${DEST}" ] && cmp -s "${SRC_FILE}" "${DEST}"; then
    echo "✅ ${DEST} already installed and up to date. Nothing to do."
    exit 0
fi

# Sanity (read-only): the drop-in is only read if the main config includes
# ssh_config.d. Warn (never edit /etc/ssh/ssh_config here); the file is
# world-readable, so no sudo is needed for this check.
if ! grep -rqE '^[[:space:]]*Include[[:space:]]+/etc/ssh/ssh_config\.d/' /etc/ssh/ssh_config 2>/dev/null; then
    echo "⚠️  /etc/ssh/ssh_config has no 'Include /etc/ssh/ssh_config.d' — the drop-in would be ignored."
    echo "   Add that line to /etc/ssh/ssh_config (then re-run cz apply)."
fi

sudo mkdir -p "${DEST_DIR}"
sudo install -o root -g root -m 0644 "${SRC_FILE}" "${DEST}"

echo "✅ ${DEST} installed."
echo "Verify with: sudo ssh -G localhost | grep -i forwardagent"
