#!/bin/bash
# run_once_5_aitools_2llama_startup.sh
#
# Install the boot-time systemd units for jan's dual llama.cpp backends, plus
# the huggingface-hub bind mount, WITHOUT a login required. Both backends are
# provisioned by this one script:
#
#   llama-cuda.service -> ~/.local/bin/restart-llama-cuda.sh   :8081 NVIDIA CUDA
#   llama-sycl.service -> ~/.local/bin/restart-llama-sycl.sh   :8082 Intel SYCL
#
# Each unit is Type=oneshot whose launcher detaches the llama-server (fork +
# disown) and exits, so systemd tracks only the launcher while the orphaned
# server keeps serving. We enable at boot but only (re)start a backend when it
# is NOT already serving on its port, so applying this script never kills a live
# instance.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

# --- 0. retire the renamed-old CUDA unit + launcher (idempotent) ------------
# Before the CUDA backend was renamed from restart-llama-server.* to
# llama-cuda.* / restart-llama-cuda.sh the old unit was still installed AND
# enabled; leaving it would let systemd start both units on :8081. Disable it
# and drop the unit file + its wants link. Sudo is scoped to the drop-in.
OLD_UNIT_NAME=restart-llama-server.service
OLD_UNIT_DEST="/etc/systemd/system/${OLD_UNIT_NAME}"
OLD_WANTS_LINK="/etc/systemd/system/multi-user.target.wants/${OLD_UNIT_NAME}"
if sudo systemctl list-unit-files "${OLD_UNIT_NAME}" 2>/dev/null | grep -q "${OLD_UNIT_NAME}"; then
    sudo systemctl disable "${OLD_UNIT_NAME}" >/dev/null 2>&1 || true
    sudo rm -f "${OLD_UNIT_DEST}" "${OLD_WANTS_LINK}"
    echo "✅ Retired old unit ${OLD_UNIT_NAME} (disabled + unit file + wants link removed)."
fi
# Drop the stale old launcher from the user tree (no sudo needed).
if [ -f "$HOME/.local/bin/restart-llama-server.sh" ]; then
    rm -f "$HOME/.local/bin/restart-llama-server.sh"
    echo "✅ Removed stale launcher ~/.local/bin/restart-llama-server.sh"
fi

# --- 1. backend table -------------------------------------------------------
# Each entry is UNIT|SCRIPT|PORT|START:
#   UNIT   systemd unit name (etc/systemd/system/<UNIT>)
#   SCRIPT home-relative launcher path (no leading ~ — `read` does not expand
#          tildes, so paths are joined with $HOME below); we only act when this
#          file exists, so a machine without the Intel SYCL build simply skips.
#          (The legacy scripts used bare `~/...` at assignment time, which is why
#          tilde expansion happened there — not to be reproduced here.)
#   PORT   backend http port, used for the "already serving?" liveness probe.
#   START  "yes" => enable + start-if-not-serving ; "no" => enable-only.
# CUDA is the primary backend: it is started on deploy. SYCL is heavy (~3.8 GB),
# so it is enabled at boot but left to an explicit `systemctl start`.
declare -a BACKENDS=(
  "llama-cuda.service|.local/bin/restart-llama-cuda.sh|8081|yes"
  "llama-sycl.service|.local/bin/restart-llama-sycl.sh|8082|no"
)

# Bail early (no sudo, no fstab change) when neither launcher is present.
have_backend=0
for entry in "${BACKENDS[@]}"; do
    IFS='|' read -r UNIT SCRIPT PORT START <<<"$entry"
    if [ -f "$HOME/${SCRIPT}" ]; then have_backend=1; fi
done
[ "${have_backend}" -eq 1 ] || {
    echo "$(basename "$0"): no llama backend launcher found — nothing to install."
    exit 0
}

# --- 2. huggingface-hub bind mount in /etc/fstab (idempotent) ---------------
# Both backends serve their model from a blob in ~/.cache/huggingface/hub, so
# the hub must be mounted at boot and the script never needs `sudo mount`.
FSTAB=/etc/fstab
MOUNT_LINE="/media/passeport/huggingface-hub/ /home/jan/.cache/huggingface/hub none bind,nofail 0 0"
if grep -Fq "/home/jan/.cache/huggingface/hub" "${FSTAB}"; then
    echo "✅ ${FSTAB} already contains the huggingface-hub bind mount."
else
    FSTAB_BAK="$(mktemp)"
    sudo cp "${FSTAB}" "${FSTAB_BAK}"
    echo "${MOUNT_LINE}" | sudo tee -a "${FSTAB}" >/dev/null
    echo "✅ Added huggingface-hub bind mount to ${FSTAB} (backup: ${FSTAB_BAK})"
fi

# --- 3. install each unit, enable, and (maybe) start ------------------------
for entry in "${BACKENDS[@]}"; do
    IFS='|' read -r UNIT SCRIPT PORT START <<<"$entry"
    SCRIPT_HOME="${HOME}/${SCRIPT}"
    # Guard: only act for a backend whose launcher is present.
    if [ ! -f "${SCRIPT_HOME}" ]; then
        echo "⏭️  skipping ${UNIT} — ${SCRIPT} not present."
        continue
    fi
    UNIT_SRC="${SRC_DIR}/etc/systemd/system/${UNIT}"
    UNIT_DEST="/etc/systemd/system/${UNIT}"
    if [ ! -f "${UNIT_SRC}" ]; then
        echo "⚠️  Unit source not found: ${UNIT_SRC}"
        exit 1
    fi
    # Install (overwrite only if changed). Both repo source and installed unit
    # are world-readable, so `cmp` runs as jan; the sudo bits are cp/rm/install.
    if [ -f "${UNIT_DEST}" ] && cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
        echo "✅ ${UNIT_DEST} already installed and up to date."
    else
        if [ -f "${UNIT_DEST}" ]; then sudo rm -f "${UNIT_DEST}"; fi
        sudo install -o root -g root -m 0644 "${UNIT_SRC}" "${UNIT_DEST}"
        echo "✅ Installed ${UNIT_DEST}"
    fi
    # Enable at boot; only start when START=yes AND nothing is serving yet.
    # Guard on the port (not `is-active`): a freshly installed oneshot unit is
    # "inactive" even while a backend already serves, so `is-active` would start
    # it and reap the live instance. `enable --now` semantics are available via
    # `sudo systemctl enable --now ${UNIT}` when a restart is actually wanted.
    sudo systemctl enable "${UNIT}"
    if [ "${START}" = "yes" ] && ! curl -fsS "http://127.0.0.1:${PORT}/v1/models" >/dev/null 2>&1; then
        sudo systemctl start "${UNIT}"
        echo "✅ ${UNIT} started (:${PORT} was not serving)."
    else
        echo "✅ ${UNIT} enabled at boot — left serving, not restarted."
    fi
done

sudo systemctl daemon-reload
echo "✅ llama backends deployed."
