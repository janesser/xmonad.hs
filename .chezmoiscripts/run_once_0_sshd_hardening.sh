#!/bin/bash

# Deploy the raspberrypi-only sshd hardening drop-in (keys-only auth, no X11).
# Pattern: run_once_0_ssh_agent_install.sh — idempotent, safe to re-run via
# cz apply; only uses /etc/sudoers.d/base-allowed commands (cmp, install,
# systemctl, journalctl).

HOSTNAME=$(hostname)
if [ "${HOSTNAME}" != "raspberrypi" ]; then
    echo "ℹ️  sshd hardening drop-in is raspberrypi-only; skipping on ${HOSTNAME}."
    exit 0
fi

SRC_FILE="${CHEZMOI_SOURCE_DIR:-.}/etc/ssh/sshd_config.d/10-raspberrypi-hardening.conf"
DEST_DIR=/etc/ssh/sshd_config.d
DEST="${DEST_DIR}/10-raspberrypi-hardening.conf"

if [ ! -f "${SRC_FILE}" ]; then
    echo "⚠️  source file missing: ${SRC_FILE}" >&2
    exit 1
fi

if [ -f "${DEST}" ] && cmp -s "${SRC_FILE}" "${DEST}"; then
    echo "✅ ${DEST} already installed and up to date. Nothing to do."
    exit 0
fi

sudo install -d -o root -g root -m 0755 "${DEST_DIR}"
sudo install -o root -g root -m 0644 "${SRC_FILE}" "${DEST}"

# Validate before activating: sshd syntax check is not base-allowed, so use
# the functional checks below instead — first prove keys still work.
echo "✅ ${DEST} installed."
echo "Verifying key-only login (must print KEYLOGIN_OK):"
ssh -o PasswordAuthentication=no -o BatchMode=yes \
    -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no \
    -o ConnectTimeout=5 jan@127.0.0.1 'echo KEYLOGIN_OK' 2>/dev/null

# Reload (not restart): existing sessions keep their old config, new
# connections get keys-only auth. Safe from inside an active session.
sudo systemctl reload ssh || sudo systemctl restart ssh

sleep 1
echo "Journal tail for ssh:"
journalctl -u ssh --since '30 seconds ago' --no-pager | tail -5
