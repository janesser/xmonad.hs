#!/bin/bash
# run_once_5_aitools_3olla_startup.sh
#
# Make Olla run at system boot WITHOUT a login required, consistent with
# restart-llama-server.service:
#
#   1. Install a SYSTEM systemd unit to /etc/systemd/system that runs Olla as
#      `jan` at boot (WantedBy=multi-user.target). A *system* unit — not a
#      user unit — is what lets it start before anyone logs in, so no
#      `loginctl enable-linger` is needed.
#   2. Enable + start it now.
#
# Both steps are idempotent and guarded. Sudo is used only for commands in the
# scoped NOPASSWD sudoers drop-in (cp, chmod, systemctl). Reading the unit for
# the up-to-date check needs no sudo — both paths are world-readable.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

# Guard: Ola is a *proxy* for the llama.cpp backend (public :40114 -> :8081) and
# its unit has Wants=/After=restart-llama-server.service. Install it only on the
# desktop that actually runs the llama backend; otherwise we'd ship a boot
# service that can never reach its target and would just loop on
# Restart=on-failure. This mirrors run_once_5_aitools_2llama_startup.sh, which
# guards the llama install on the same launcher below. Sudo is used only for
# commands in the scoped NOPASSWD sudoers drop-in.
SCRIPT=~/.local/bin/restart-llama-server.sh
if [ ! -f "${SCRIPT}" ]; then
    echo "$(basename "$0"): ${SCRIPT} not found — skipping Ola startup install."
    exit 0
fi

UNIT_NAME=olla.service
UNIT_SRC="${SRC_DIR}/etc/systemd/system/${UNIT_NAME}"
UNIT_DEST="/etc/systemd/system/${UNIT_NAME}"

if [ ! -f "${UNIT_SRC}" ]; then
    echo "⚠️  Unit source not found: ${UNIT_SRC}"
    exit 1
fi

# --- 1. install unit (overwrite only if changed) ----------------------------
# Both the repo source and the installed unit are world-readable (0644), so
# `cmp` runs as jan without sudo. `cp`/`chmod` below are the sudo bits and are
# the only privileged commands (allowed by the chezmoi-pi sudoers drop-in).
if [ -f "${UNIT_DEST}" ] && cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
    echo "✅ ${UNIT_DEST} already installed and up to date."
else
    sudo cp "${UNIT_SRC}" "${UNIT_DEST}"
    sudo chmod 0644 "${UNIT_DEST}"
    echo "✅ Installed ${UNIT_DEST}"
fi

sudo systemctl daemon-reload

# --- 2. enable + start now --------------------------------------------------
sudo systemctl enable --now "${UNIT_NAME}"
echo "✅ ${UNIT_NAME} enabled and started — it runs at every boot, no login required."
