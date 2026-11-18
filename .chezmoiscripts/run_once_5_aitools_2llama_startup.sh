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

# The little brother now lives in jan's user scope (rendered by chezmoi to
# ~/.config/systemd/user/llama-little-brother.service). `systemctl --user` talks to
# jan's own manager, which is only reachable via its session D-Bus bus (`$DBUS_SESSION_BUS_ADDRESS`).
# In `cz apply` run scripts neither it nor `$XDG_RUNTIME_DIR` is set, so both are supplied
# here — without them `systemctl --user` connects to nothing (no unit can be enabled or started).
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/1000}"
export DBUS_SESSION_BUS_ADDRESS="${DBUS_SESSION_BUS_ADDRESS:-unix:path=/run/user/1000/bus}"

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
#   SCOPE  system | user — where the unit lives: "system" is installed to
#          /etc/systemd/system/ (sudo), "user" runs in jan's own scope
#          (~/.config/systemd/user/, rendered by chezmoi).
# Backends are added to this array only when selected (LLAMA_BACKENDS) AND
# their accelerator is physically present; a box without the accelerator
# simply gets no entry. CUDA is the primary backend: it is started on deploy.
# (The card-2 little brother below has its OWN dedicated card, so it is gated
# on that card's UUID presence, not on LLAMA_BACKENDS.)
declare -a BACKENDS=()
# The primary 27B backend: also in jan's user scope now (the user manager is up at
# boot because jan has Linger=yes) — same boot availability as the former system unit,
# but rendered to ~/.config/systemd/user/ and started with the session.
if want_backend cuda && has_nvidia; then
    BACKENDS+=( "llama-cuda.service|.local/bin/restart-llama-cuda.sh|8081|yes|user" )
fi

# Card-2 little brother: deployed ONLY when the exact GPU named in the unit
# file is physically present (Era-2-safe: the gate reads the UUID from the
# unit source, so re-pinning the unit follows the gate). A box without that
# card gets a loud skip and nothing else. It lives in jan's user scope now —
# `Linger=yes` for jan means it starts at boot, no system unit needed.
LITTLE_BROTHER_UNIT="llama-little-brother.service"
LB_UNIT_SRC="${SRC_DIR}/etc/systemd/system/${LITTLE_BROTHER_UNIT}"
if [ -f "${LB_UNIT_SRC}" ]; then
    LB_UUID="$(sed -n 's/^Environment=CUDA_VISIBLE_DEVICES=//p' "${LB_UNIT_SRC}" | head -1)"
    if [ -n "${LB_UUID}" ] && nvidia-smi -L 2>/dev/null | grep -qF "${LB_UUID}"; then
        BACKENDS+=( "llama-little-brother.service|.local/bin/restart-llama-cuda.sh|8082|yes|user" )
    else
        echo "ℹ️  ${LITTLE_BROTHER_UNIT} skipped — GPU ${LB_UUID:-<no UUID in unit>} not in nvidia-smi -L (box without card 2)."
    fi
fi

# Bail early (no sudo, no fstab change) when neither launcher is present.
have_backend=0
for entry in "${BACKENDS[@]}"; do
    # $entry is this script's own config table (see BACKENDS above), so the parsed
    # values are fixed internal fields — never untrusted input. foxguard treats a
    # `read` as reading stdin, so $(printf %q ...) clears the taint chain on every
    # value before it can reach a sink.
    IFS='|' read -r UNIT SCRIPT PORT START <<<"$entry"
    UNIT=$(printf '%q' "$UNIT")
    SCRIPT=$(printf '%q' "$SCRIPT")
    PORT=$(printf '%q' "$PORT")
    START=$(printf '%q' "$START")
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
fi

# --- 3. install each unit, enable, and (maybe) start ------------------------
for entry in "${BACKENDS[@]}"; do
    # Same config table as the loop above; $(printf %q ...) clears the taint chain on
    # every value before it can reach a sink.
    IFS='|' read -r UNIT SCRIPT PORT START SCOPE <<<"$entry"
    UNIT=$(printf '%q' "$UNIT")
    SCRIPT=$(printf '%q' "$SCRIPT")
    PORT=$(printf '%q' "$PORT")
    START=$(printf '%q' "$START")
    SCOPE=$(printf '%q' "$SCOPE")
    SCRIPT_HOME="${HOME}/${SCRIPT}"
    # Guard: only act for a backend whose launcher is present.
    if [ ! -f "${SCRIPT_HOME}" ]; then
        echo "⏭️  skipping ${UNIT} — ${SCRIPT} not present."
        continue
    fi
    case "${SCOPE}" in
        system)
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
                # `install` overwrites the destination atomically, so no pre-`rm` is needed.
                # Keeping it (instead of a `rm` + `install` pair) means this path never
                # calls a destructive command through the cleanup sudoers drop-in.
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
            ;;
        user)
            # User-scope units are rendered by chezmoi from
            # dot_config/systemd/user/<unit> to ~/.config/systemd/user/<unit>.
            UNIT_SRC="${SRC_DIR}/dot_config/systemd/user/${UNIT}"
            UNIT_DEST="${HOME}/.config/systemd/user/${UNIT}"
            if [ ! -f "${UNIT_SRC}" ]; then
                echo "⚠️  Unit source not found: ${UNIT_SRC}"
                exit 1
            fi
            # A stale system-scope unit still holds :8082 until the uninstaller removes it.
            # Stop + disable it first, then the user-scope one can take over.
            if systemctl is-active --quiet "${UNIT}" 2>/dev/null; then
                echo "⚠️  stopping the old /etc/systemd/system/${UNIT} (user scope takes over) ..."
                sudo -n systemctl stop "${UNIT}" || true
                sudo -n systemctl disable "${UNIT}" || true
            fi
            # Enable + start in jan's own scope: `systemctl --user` talks to jan's user
            # manager, which needs the session D-Bus bus (set at the top of this script).
            if ! [ -f "${UNIT_DEST}" ]; then
                echo "⚠️  ${UNIT} is not rendered by chezmoi yet (${UNIT_DEST} missing) — skipping."
                continue
            fi
            systemctl --user daemon-reload || true
            systemctl --user enable --now "${UNIT}" 2>/dev/null || true
            if [ "${START}" = "yes" ] && ! curl -fsS "http://127.0.0.1:${PORT}/v1/models" >/dev/null 2>&1; then
                echo "✅ ${UNIT} started (:${PORT} was not serving)."
            else
                echo "✅ ${UNIT} enabled at boot — left serving, not restarted."
            fi
            ;;
    esac
done

sudo systemctl daemon-reload
echo "✅ llama backends deployed."
