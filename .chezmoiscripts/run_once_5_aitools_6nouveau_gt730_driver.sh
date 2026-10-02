#!/bin/bash
# run_once_5_aitools_6nouveau_gt730_driver.sh
#
# Install the systemd oneshot that loads the open-source nouveau KMS driver for
# the GeForce GT 730 (GK208B/Maxwell) on cyberkleiber's dual-driver box.
#
#   nouveau-gt730.service -> /usr/local/bin/load-nouveau-gt730.sh
#
# WHY nouveau: the GT 730 is rejected by the current nvidia driver, and the
# legacy 470.xx branch cannot coexist with the 580 driver on the V100 (single
# global nvidia kernel module). So the split is GT730 -> nouveau, V100 -> nvidia.
#
# nouveau needs mxm_wmi loaded first (it exports the mxm_wmi_* symbols nouveau
# requires); the wrapper checks that. `modprobe nouveau` mis-resolves to "off"
# on this host, so the wrapper insmod-by-path instead.
#
# HARDWARE GUARD: deploy ONLY where BOTH a GT 730 (GK208B, 10de:1287) AND a
# Tesla V100 (GV100, 10de:1db5) are present — that is the only topology where
# nouveau-on-GT730 is the right call. Skips cleanly (no sudo, no install) elsewhere.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
source "$HOME/.local/share/gpu.func"

UNIT=nouveau-gt730.service
UNIT_SRC="${SRC_DIR}/etc/systemd/system/${UNIT}"
UNIT_DEST="/etc/systemd/system/${UNIT}"
LAUNCHER=/usr/local/bin/load-nouveau-gt730.sh
LAUNCHER_SRC="${SRC_DIR}/usr_local/bin/load-nouveau-gt730.sh"

# --- 1. hardware guard: deploy only where a GT730 AND a V100 coexist ---------
if ! has_nvidia_gt730 || ! has_v100; then
    echo "$(basename "$0"): no GT730+V100 combo present — skipping nouveau-for-GT730 driver."
    exit 0
fi

# --- 2. install the launcher (idempotent) -----------------------------------
# `usr/` is chezmoi-ignored, so this source is consumed only by run scripts,
# never symlinked. It has no `{{ }}`, so chezmoi copies it verbatim.
if [ ! -f "${LAUNCHER_SRC}" ]; then
    echo "⚠️  launcher source not found: ${LAUNCHER_SRC}"
    exit 1
fi
if [ -f "${LAUNCHER}" ] && cmp -s "${LAUNCHER_SRC}" "${LAUNCHER}"; then
    echo "✅ ${LAUNCHER} already installed and up to date."
else
    sudo install -o root -g root -m 0755 "${LAUNCHER_SRC}" "${LAUNCHER}"
    echo "✅ Installed ${LAUNCHER}"
fi

# --- 3. install the unit, enable at boot ------------------------------------
# `etc/` is chezmoi-ignored too; this is a plain unit (no `{{ }}`).
if [ ! -f "${UNIT_SRC}" ]; then
    echo "⚠️  unit source not found: ${UNIT_SRC}"
    exit 1
fi
if [ -f "${UNIT_DEST}" ] && cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
    echo "✅ ${UNIT_DEST} already installed and up to date."
else
    sudo install -o root -g root -m 0644 "${UNIT_SRC}" "${UNIT_DEST}"
    echo "✅ Installed ${UNIT_DEST}"
fi
sudo systemctl enable "${UNIT}"
sudo systemctl daemon-reload
echo "✅ nouveau-for-GT730 driver deployed (enabled at boot)."
