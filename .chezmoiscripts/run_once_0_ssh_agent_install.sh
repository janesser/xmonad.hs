#!/bin/bash

command -v apt && sudo apt install -y ssh-agent
# sudo pacman -Fy ssh-agent
command -v pacman && sudo pacman -Sy --noconfirm openssh

echo "✅ ssh-agent installed."


SRC_FILE="${CHEZMOI_SOURCE_DIR:-.}/etc/ssh/ssh_config.d/99-allow-agent-forward.conf"
DEST_DIR=/etc/ssh/ssh_config.d
DEST="${DEST_DIR}/99-allow-agent-forward.conf"

if [ -f "${DEST}" ] && cmp -s "${SRC_FILE}" "${DEST}"; then
    echo "✅ ${DEST} already installed and up to date. Nothing to do."
    exit 0
fi

sudo install -d -o root -g root -m 0755 "${DEST_DIR}"
sudo install -o root -g root -m 0644 "${SRC_FILE}" "${DEST}"

echo "✅ ${DEST} installed."
echo "Verify with: sudo ssh -G localhost | grep -i forwardagent"
