#!/bin/bash
# run_once_5_aitools_2llama_startup.sh
#
# Make ~/.local/bin/restart-llama-server.sh start at system boot, WITHOUT a
# login required:
#
#   1. Install a SYSTEM systemd unit to /etc/systemd/system that runs the
#      script as `jan` at boot (WantedBy=multi-user.target). A *system* unit —
#      not a user unit — is what lets it start before anyone logs in, so no
#      `loginctl enable-linger` is needed.
#   2. Add the huggingface-hub bind-mount to /etc/fstab so the hub is already
#      mounted at boot and the script never needs `sudo mount` (which has no
#      password here).
#
# Both installs are idempotent and guarded: this only does anything on the
# desktop that actually ships restart-llama-server.sh + the passeport drive.
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in
# (install, rm, tee, cp, systemctl).

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
SCRIPT=~/.local/bin/restart-llama-server.sh

# Guard: only on machines that actually run llama (desktop with passeport).
if [ ! -f "${SCRIPT}" ]; then
    echo "$(basename "$0"): ${SCRIPT} not found — skipping llama startup install."
    exit 0
fi

# --- 1. systemd system unit -------------------------------------------------
UNIT_NAME=restart-llama-server.service
UNIT_SRC="${SRC_DIR}/systemd/system/${UNIT_NAME}"
UNIT_DEST="/etc/systemd/system/${UNIT_NAME}"

if [ ! -f "${UNIT_SRC}" ]; then
    echo "⚠️  Unit source not found: ${UNIT_SRC}"
    exit 1
fi

if [ -f "${UNIT_DEST}" ] && sudo cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
    echo "✅ ${UNIT_DEST} already installed and up to date."
else
    if [ -f "${UNIT_DEST}" ]; then sudo rm -f "${UNIT_DEST}"; fi
    sudo install -o root -g root -m 0644 "${UNIT_SRC}" "${UNIT_DEST}"
    echo "✅ Installed ${UNIT_DEST}"
fi

sudo systemctl daemon-reload

# --- 2. huggingface-hub bind mount in /etc/fstab ----------------------------
FSTAB=/etc/fstab
MOUNT_LINE="/media/passeport/huggingface-hub/ /home/jan/.cache/huggingface/hub none bind,nofail 0 0"

if grep -Fq "/home/jan/.cache/huggingface/hub" "${FSTAB}"; then
    echo "✅ ${FSTAB} already contains the huggingface-hub bind mount."
else
    FSTAB_BAK="$(mktemp)"
    sudo cp "${FSTAB}" "${FSTAB_BAK}"
    echo "${MOUNT_LINE}" | sudo tee -a "${FSTAB}" >/dev/null
    echo "✅ Added huggingface-hub bind mount to ${FSTAB} (fstab backup: ${FSTAB_BAK})"
fi

# --- 3. enable + start now --------------------------------------------------
sudo systemctl enable --now "${UNIT_NAME}"
echo "✅ ${UNIT_NAME} enabled and started — it runs at every boot, no login required."
