#!/bin/bash
# run_once_5_aitools_4llama_sycl_startup.sh
#
# Make ~/.local/bin/restart-llama-sycl.sh start at system boot, WITHOUT a login
# required, by installing a SYSTEM systemd unit (llama-sycl.service) that runs
# the llama.cpp SYCL (Intel-GPU) backend at boot. Mirrors
# run_once_5_aitools_2llama_startup.sh (the CUDA backend).
#
# Idempotent + guarded: only installs on a machine that ships the launcher.
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in
# (install, rm, tee, cp, systemctl).

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
SCRIPT=~/.local/bin/restart-llama-sycl.sh

# Guard: only on machines that actually run llama on the Intel GPU.
if [ ! -f "${SCRIPT}" ]; then
    echo "$(basename "$0"): ${SCRIPT} not found — skipping SYCL llama startup install."
    exit 0
fi

# --- 1. systemd system unit -------------------------------------------------
UNIT_NAME=llama-sycl.service
UNIT_SRC="${SRC_DIR}/etc/systemd/system/${UNIT_NAME}"
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

# --- 2. enable for boot (resilient across restarts) -------------------------
# NOTE: not --now on purpose: loading the ~3.8 GB gemma model is heavy, so we
# leave the first start to an explicit `systemctl start` (or next boot) and let
# the launcher's memory-guard decide whether it is safe to load right now.
sudo systemctl enable "${UNIT_NAME}"
echo "✅ ${UNIT_NAME} enabled at boot — start it any time with `systemctl --system start ${UNIT_NAME}`."
