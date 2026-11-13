#!/bin/bash
# run_once_5_aitools_8signal_pi.sh
#
# Deploy the Signal <-> pi persistent agent on raspberrypi:
#
#   signal-receive.service    /etc/systemd/system/signal-receive.service
#     (the signal-cli daemon, already provisioned by the pi-signal extension's
#      setup wizard on this box — just moved into the repo for idempotent
#      management)
#   signal-pi.service         /etc/systemd/system/signal-pi.service
#     (pi --print with PI_SIGNAL_* env vars — the extension consumes the SSE
#      stream, injects messages into the agent session and auto-replies via the
#      daemon JSON-RPC API; no separate listener script is needed)
#
# Both units are raspberrypi-only: one Signal account runs on one host. On any
# other host this script logs and exits 0.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in
# (see the repo AGENTS.md).

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
UNIT_RECEIVE=signal-receive.service
UNIT_PI=signal-pi.service
UNIT_RECEIVE_SRC="${SRC_DIR}/etc/systemd/system/${UNIT_RECEIVE}"
UNIT_PI_SRC="${SRC_DIR}/etc/systemd/system/${UNIT_PI}"
UNIT_RECEIVE_DEST="/etc/systemd/system/${UNIT_RECEIVE}"
UNIT_PI_DEST="/etc/systemd/system/${UNIT_PI}"

# --- 1. host gate (raspberrypi only) -------------------------------------------
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "⏭️ $(hostname) is not raspberrypi — signal-pi only runs there."
    exit 0
fi

# --- 2. install units ------------------------------------------------------------
# `install` overwrites the destination atomically, so no pre-`rm` is needed.
# Compare first: on unchanged applies this path stays a no-op (no `sudo`, no
# prompt).

if [ -f "${UNIT_RECEIVE_DEST}" ] && cmp -s "${UNIT_RECEIVE_SRC}" "${UNIT_RECEIVE_DEST}"; then
    echo "✅ ${UNIT_RECEIVE_DEST} already installed and up to date."
else
    [ -f "${UNIT_RECEIVE_SRC}" ] || { echo "⚠️  unit source not found: ${UNIT_RECEIVE_SRC}"; exit 1; }
    sudo install -o root -g root -m 0644 "${UNIT_RECEIVE_SRC}" "${UNIT_RECEIVE_DEST}"
    echo "✅ installed ${UNIT_RECEIVE_DEST}"
fi

if [ -f "${UNIT_PI_DEST}" ] && cmp -s "${UNIT_PI_SRC}" "${UNIT_PI_DEST}"; then
    echo "✅ ${UNIT_PI_DEST} already installed and up to date."
else
    [ -f "${UNIT_PI_SRC}" ] || { echo "⚠️  unit source not found: ${UNIT_PI_SRC}"; exit 1; }
    sudo install -o root -g root -m 0644 "${UNIT_PI_SRC}" "${UNIT_PI_DEST}"
    echo "✅ installed ${UNIT_PI_DEST}"
fi

# --- 3. activate ------------------------------------------------------------------
sudo systemctl daemon-reload
sudo systemctl enable --now "${UNIT_RECEIVE}"
sudo systemctl enable --now "${UNIT_PI}"

echo "✅ signal-pi deployment complete: $(date +%Y-%m-%dT%H:%M:%S)"
