#!/bin/bash
# run_once_5_aitools_3btop_intel_gpu_cap.sh
#
# Make btop surface Intel Iris Xe GPU usage (and, in general, any i915-based
# integrated GPU). Two things are required, both done here:
#
#   1. A patched btop binary. btop 1.4.6 hardcodes the i915 perf-PMU sysfs path
#      to /sys/devices/i915/events, but recent kernels instantiate the device by
#      its PCI address (e.g. /sys/devices/i915_0000_2f_00.0/events). btop then
#      fails to discover the engines and the Iris Xe box silently vanishes — and
#      on DG1 / IGPUs (which expose no RAPL energy-gpu PMU) power + temperature
#      must be read from the i915 hwmon sensor instead. The fix lives committed
#      in the full ~/projs/btop clone (branch btop-intel-gpu-fix): discover_engines()
#      resolves the concrete i915_* device by scanning /sys/devices and
#      propagates the instance name into engines->device, and hwmon power/temperature
#      is read and sampled for instantaneous power. We build that committed fix
#      from source and install it to ~/.local/bin/btop.
#
#   2. CAP_PERFMON on ~/.local/bin/btop. btop attaches to the i915 perf PMU via
#      perf_event_open(); a normal user gets EPERM unless it holds CAP_PERFMON
#      (kernel.perf_event_paranoid >= 4 blocks it). No external package is
#      needed — just the capability on the binary.
#
# REPRODUCIBILITY (single source of truth):
#   The build uses a CLEAN, DEDICATED clone of the fix branch (see BTOP_DIR
#   below), NOT the developer's working clone at ~/projs/btop. Reasons:
#     * A fresh box has no fix anywhere, so we clone the fork's fix branch from
#       the network (the dev clone, or the pinned patch, is NOT assumed to
#       exist). Cloning upstream aristocratos/btop would yield a vanilla binary
#       with no fix.
#     * The developer's working clone may be mid-rebase / dirty; a dirty tree
#       can produce a broken build. The dedicated clone is always clean and is
#       kept current with `git fetch` + checkout, so the deploy is deterministic
#       for a given fork state.
#   The fork branch is `btop-intel-gpu-fix`. This is the one place to move when
#   the fix advances — push the branch to janesser and the next cz update builds
#   it. No separate pinned SHA to hand-edit.
#
# SAFEGUARD "where required":
#   Only run this on a box that actually has an Intel GPU (has_intel_gpu()).
#   On ARM / NVIDIA-only / headless machines we touch nothing, so CAP_PERFMON
#   is never handed out unnecessarily. GPU detection is centralised in
#   ~/.local/share/gpu.func (sourced below) — the same never-false-negative
#   lspci presence test used by the llama.cpp build script.
#
# POLICY (vanilla vs. patched btop):
#   This script installs the i915-patched btop ONLY on a host that actually
#   has an Intel GPU. On any other host (NVIDIA-only, ARM, headless) it does
#   nothing: the vanilla system btop package is left exactly as-is, and no
#   cap_perfmon is handed out. Two layers enforce this:
#     1. Top-level guard: `has_intel_gpu` -> log + exit 0 on a non-Intel host.
#     2. ensure_btop() assertion: refuses to install if has_intel_gpu is false
#        (defence in depth — patched btop is only ever produced on an Intel
#        GPU host, never handed out unnecessarily).
#
# Root usage: `install` to ~/.local/bin is user-owned and needs NO sudo. The
# only privileged step is `setcap`, and because the binary lives under the
# user's home (not /usr/bin/btop) it is NOT covered by the scoped NOPASSWD
# setcap rule (which only ever named /usr/bin/btop). So this needs jan's normal
# sudo password (the `(ALL:ALL) ALL` rule), not the passwordless drop-in.
#
# Build steps (git/cmake/make) run as the user (no sudo needed). This script is
# run_once, so the (slow) build happens on the first cz update; subsequent
# updates are no-ops unless the fork branch advances (tracked by a stamp). The
# setcap step is gated by a getcap check, so once the cap is granted it is
# never requested again.

set -o pipefail

# Dedicated clean clone of the fix branch (separate from the developer's working
# clone at ~/projs/btop).
BTOP_DIR="$HOME/projs/btop-intel-gpu"
BTOP_BRANCH="btop-intel-gpu-fix"
# Fork is reachable over SSH (matches the dev clone's janesser auth); HTTPS is a
# fallback for a fresh box that hasn't set up its key yet.
BTOP_FORK_SSH="git@github.com:janesser/btop.git"
BTOP_FORK_HTTPS="https://github.com/janesser/btop.git"
# Stamp of the fork HEAD that is currently installed at ~/.local/bin/btop.
BTOP_STAMP="$HOME/.local/share/btop_deployed_commit"

# Detection + the capability helper live in gpu.func (rendered to
# ~/.local/share by chezmoi before run scripts execute).
source "$HOME/.local/share/gpu.func"

log() { echo "$(basename "$0"): $*"; }

# ensure_btop -> build the i915/DG1-patched btop from the dedicated clone and
# install it to ~/.local/bin/btop. Returns non-zero on failure (logged, never
# fatal, so a failed build during cz update leaves the working system btop in
# place for a retry on the next update).
ensure_btop() {
    if ! has_intel_gpu; then
        log "REFUSING to install patched btop: no Intel GPU on this host; keeping vanilla system btop."
        return 1
    fi

    if [ ! -d "$BTOP_DIR/.git" ]; then
        log "Cloning btop fix branch '$BTOP_BRANCH' -> $BTOP_DIR"
        if ! git clone --branch "$BTOP_BRANCH" "$BTOP_FORK_SSH" "$BTOP_DIR" 2>/dev/null; then
            log "SSH clone failed; trying HTTPS."
            if ! git clone --branch "$BTOP_BRANCH" "$BTOP_FORK_HTTPS" "$BTOP_DIR"; then
                log "ERROR: could not clone btop fix branch (check network/auth) and re-run."
                return 1
            fi
        fi
    fi

    # Keep the dedicated clone current WITHOUT disturbing the developer's clone.
    # A failed fetch is non-fatal — we build whatever is already checked out.
    if ! ( cd "$BTOP_DIR" && git fetch origin --prune 2>/dev/null \
                && git checkout -B "$BTOP_BRANCH" "origin/$BTOP_BRANCH" 2>/dev/null ); then
        log "WARNING: could not update $BTOP_DIR (continuing with the local checkout)."
    fi

    local head head_short
    head="$(git -C "$BTOP_DIR" rev-parse HEAD 2>/dev/null)" \
        || { log "ERROR: $BTOP_DIR is not a git repo"; return 1; }
    head_short="$(git -C "$BTOP_DIR" rev-parse --short HEAD 2>/dev/null)"

    if [ ! -x "$BTOP_DIR/build/btop" ] || [ "$(cat "$BTOP_STAMP" 2>/dev/null)" != "$head" ]; then
        local ncpus
        ncpus="$(grep -c '^processor' /proc/cpuinfo 2>/dev/null || echo 4)"
        log "Building btop ($head_short, $ncpus cores) ..."
        if ! ( cd "$BTOP_DIR" && cmake -B build . \
                    -DCMAKE_BUILD_TYPE=Release \
                && cmake --build build -j "$ncpus" ); then
            log "ERROR: btop build failed — fix the above and re-run cz update."
            return 1
        fi
        # Stamp only after a successful build so a broken HEAD can never advance
        # the stamp and mask a stale binary as "up to date".
        printf '%s' "$head" > "$BTOP_STAMP"
        log "built btop $head_short"
    else
        log "btop $head_short already built; reusing."
    fi

    # Install into the user's local bin. This path is user-owned, so no root is
    # needed here (the old design installed over /usr/bin/btop).
    if ! install -m755 "$BTOP_DIR/build/btop" "$HOME/.local/bin/btop"; then
        log "ERROR: could not install patched btop to ~/.local/bin/btop."
        return 1
    fi
    log "installed patched btop ($head_short) to ~/.local/bin/btop"
    return 0
}

if ! has_intel_gpu; then
    # Non-Intel host: use the vanilla system btop untouched. No build, no
    # install, no cap_perfmon grant, no changes to /usr/bin/btop.
    log "No Intel GPU on this host — using vanilla system btop (no patch, no cap grant)."
    exit 0
fi

ensure_btop || log "btop patch/install was not applied — see errors above; re-run cz update."

# Grant CAP_PERFMON so btop can attach to the i915 perf PMU (perf_event_paranoid
# is high on this box, so the cap is required for the DG1 engine/power counters).
# The binary is user-owned, so this needs a sudo password — the scoped NOPASSWD
# setcap rule only ever covered /usr/bin/btop, which we no longer touch. The
# getcap check makes it idempotent: once the cap is set, future cz updates do
# not prompt. If it fails here (e.g. headless cz update), grant it once manually:
#   sudo setcap cap_perfmon+ep ~/.local/bin/btop
if getcap "$HOME/.local/bin/btop" 2>/dev/null | grep -q 'cap_perfmon'; then
    log "cap_perfmon already on ~/.local/bin/btop."
elif sudo setcap cap_perfmon+ep "$HOME/.local/bin/btop"; then
    log "granted cap_perfmon on ~/.local/bin/btop."
else
    log "WARNING: could not grant cap_perfmon to ~/.local/bin/btop — i915 engine/power counters won't be readable until it is."
fi

log "Done. Restart btop to see the Intel Iris Xe GPU box."
