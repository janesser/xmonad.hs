#!/bin/bash
# run_once_5_aitools_2llama_startup.sh
#
# Install the boot-time systemd units for jan's llama.cpp text backends,
# WITHOUT a login required:
#
#   llama-cuda.service           -> ~/.local/bin/restart-llama-cuda.sh
#                                   :8081, the 27B on the shared V100 (SXM2)
#   llama-little-brother.service -> ~/.local/bin/start-little-brother.sh
#                                   :8082, the "little brother" on the dedicated
#                                   PCIe V100 (card 2, UUID-pinned in the unit
#                                   file; deployed ONLY when that exact card is
#                                   present)
#
# Each unit is Type=exec: the launcher reaps a stale orphan on its port, then
# `exec`s llama-server in the foreground, so systemd tracks the server itself as
# the main process and `systemctl stop` (control-group) actually kills it.
# We enable at boot but only (re)start a backend when it is NOT already serving
# on its port, so applying this script never kills a live instance.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

# GPU-presence helpers (has_nvidia / has_intel_gpu). Sourced, not executed.
source "$CHEZMOI_SOURCE_DIR/commons/gpu-detect.sh"

# --- 1. backend selection (configurable) ------------------------------------
# LLAMA_BACKENDS is a space-separated list of backends to deploy ("cuda").
# It defaults to what the box actually has; override it in the environment to
# force a subset.
#
# HARD GATE: the environment override only *selects*; physical presence
# (has_nvidia) still *authorizes*. The backend is deployed only when it is both
# selected AND its accelerator is present. So an empty LLAMA_BACKENDS on an
# NVIDIA-less box does nothing — we never hand a CUDA build to a box without
# CUDA (this is what used to deploy llama-cuda onto peacewagon, an Intel-only
# laptop).
read -ra _want_backend_arr <<< "${LLAMA_BACKENDS:-$(has_nvidia && echo cuda)}"
want_backend() {
    local b="$1" x
    for x in ${_want_backend_arr[@]}; do [ "$x" = "$b" ] && return 0; done
    return 1
}

# --- 1. backend table -------------------------------------------------------
# Each entry is UNIT|SCRIPT|PORT|START:
#   UNIT   systemd unit name (etc/systemd/system/<UNIT>)
#   SCRIPT home-relative launcher path (no leading ~ — `read` does not expand
#          tildes, so paths are joined with $HOME below); we only act when this
#          file exists.
#   PORT   backend http port, used for the "already serving?" liveness probe.
#   START  "yes" => enable + start-if-not-serving ; "no" => enable-only.
# Backends are added to this array only when selected (LLAMA_BACKENDS) AND
# their accelerator is physically present; a box without the accelerator
# simply gets no entry. CUDA is the primary backend: it is started on deploy.
# (The card-2 little brother below has its OWN dedicated card, so it is gated
# on that card's UUID presence, not on LLAMA_BACKENDS.)
declare -a BACKENDS=()
# CUDA (NVIDIA) only where there is an NVIDIA GPU.
if want_backend cuda && has_nvidia; then
    BACKENDS+=( "llama-cuda.service|.local/bin/restart-llama-cuda.sh|8081|yes" )
fi

# Card-2 little brother: deployed ONLY when the exact GPU named in the unit
# file is physically present (Era-2-safe: the gate reads the UUID from the
# unit source, so re-pinning the unit follows the gate). A box without that
# card gets a loud skip and nothing else.
LITTLE_BROTHER_UNIT="llama-little-brother.service"
LB_UNIT_SRC="${SRC_DIR}/etc/systemd/system/${LITTLE_BROTHER_UNIT}"
if [ -f "${LB_UNIT_SRC}" ]; then
    LB_UUID="$(sed -n 's/^Environment=CUDA_VISIBLE_DEVICES=//p' "${LB_UNIT_SRC}" | head -1)"
    if [ -n "${LB_UUID}" ] && nvidia-smi -L 2>/dev/null | grep -qF "${LB_UUID}"; then
        BACKENDS+=( "llama-little-brother.service|.local/bin/start-little-brother.sh|8082|yes" )
    else
        echo "ℹ️  ${LITTLE_BROTHER_UNIT} skipped — GPU ${LB_UUID:-<no UUID in unit>} not in nvidia-smi -L (box without card 2)."
    fi
fi

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

# --- 2. retire the dead SYCL text backend (idempotent one-time cleanup) -----#
# llama-sycl.service (the old Intel SYCL backend on :8082) is retired: the box
# no longer has an Intel GPU. Remove any leftover unit so it cannot start and
# hold :8082 (the port now belongs to the card-2 little brother).
if [ -f /etc/systemd/system/llama-sycl.service ]; then
    sudo systemctl disable --now llama-sycl 2>/dev/null || true
    sudo rm -f /etc/systemd/system/llama-sycl.service
    echo "✅ retired leftover llama-sycl.service"
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
