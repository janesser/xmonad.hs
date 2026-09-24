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
#      in the ~/projs/btop clone (branch btop-intel-gpu-fix): discover_engines()
#      resolves the concrete i915_* device by scanning /sys/devices and
#      propagates the instance name into engines->device, and hwmon power/temperature
#      is read and sampled for instantaneous power. We build that committed fix
#      from source and install it over /usr/bin/btop.
#
#   2. CAP_PERFMON on /usr/bin/btop. btop attaches to the i915 perf PMU via
#      perf_event_open(); a normal user gets EPERM unless it holds CAP_PERFMON
#      (kernel.perf_event_paranoid >= 4 blocks it). No external package is
#      needed — just the capability on the binary.
#
# SOURCE OF TRUTH:
#   The i915/DG1 fix is developed and committed in the full ~/projs/btop clone
#   (branch btop-intel-gpu-fix). This script builds that clone — it does NOT
#   pin a btop tag and apply a separate patch, so there is a single place the
#   fix lives. The build dir is reused; the script rebuilds whenever the clone's
#   HEAD advances (tracked by a stamp file), so a moving branch always deploys a
#   fresh binary.
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
#     2. ensure_btop() assertion: refuses to overwrite /usr/bin/btop if
#        has_intel_gpu is false (defence in depth, so a vanilla btop can never
#        be clobbered even if layer 1 were ever bypassed).
#
# Root is only ever used for commands allowed by the scoped chezmoi-pi sudoers
# drop-in (see AGENTS.md): `install` (to place the binary) and
# `/usr/sbin/setcap cap_perfmon+ep /usr/bin/btop` (exact match). NOTE: setcap
# lives in /usr/sbin, not /usr/bin, so the sudoers rule must name the resolved
# path /usr/sbin/setcap — sudo matches on the resolved path.
#
# Build steps (git/cmake/make) run as the user (no sudo needed); only the
# final install + setcap use scoped sudo. This script is run_once, so the
# (slow) build happens on the first cz update; subsequent updates are no-ops
# until the clone's HEAD moves.

set -o pipefail

BTOP_DIR="$HOME/projs/btop"
# Stamp of the clone HEAD that is currently installed at /usr/bin/btop. Rebuild
# whenever the clone advances past this commit.
BTOP_STAMP="$HOME/.local/share/btop_deployed_commit"

# Detection + the capability helper live in gpu.func (rendered to
# ~/.local/share by chezmoi before run scripts execute).
source "$HOME/.local/share/gpu.func"

log() { echo "$(basename "$0"): $*"; }

# ensure_btop -> build the i915/DG1-patched btop from the ~/projs/btop clone and
# install it over the system binary. Returns non-zero on failure (logged, never
# fatal, so a failed build during cz update leaves the working system btop in
# place for a retry on the next update).
#
# POLICY: this binary is ONLY installed where an Intel GPU is present (see the
# top-level guard below). This assertion is defence in depth — it makes it
# impossible to clobber a vanilla system btop on a non-Intel host even if the
# top-level guard were ever removed. On NVIDIA-only / ARM / headless hosts we
# leave the vanilla btop package exactly as it is.
ensure_btop() {
    if ! has_intel_gpu; then
        log "REFUSING to install patched btop: no Intel GPU on this host; keeping vanilla system btop."
        return 1
    fi

    if [ ! -d "$BTOP_DIR/.git" ]; then
        # A full clone is preferred over a shallow one so MR/inspection works
        # locally; we only clone if there is no .git yet, so cz update needs no
        # network when the clone already exists.
        log "Cloning btop -> $BTOP_DIR"
        if ! git clone https://github.com/aristocratos/btop.git "$BTOP_DIR"; then
            log "ERROR: could not clone btop (check network) and re-run."
            return 1
        fi
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

    # Install over the system binary (scoped sudo 'install').
    if ! sudo install -m755 "$BTOP_DIR/build/btop" /usr/bin/btop; then
        log "ERROR: could not install patched btop to /usr/bin/btop."
        return 1
    fi
    log "installed patched btop ($head_short) to /usr/bin/btop"
    return 0
}

if ! has_intel_gpu; then
    # Non-Intel host: use the vanilla system btop untouched. No build, no
    # install, no cap_perfmon grant, no changes to /usr/bin/btop.
    log "No Intel GPU on this host — using vanilla system btop (no patch, no cap grant)."
    exit 0
fi

ensure_btop || log "btop patch/install was not applied — see errors above; re-run cz update."

# Grant /usr/bin/btop CAP_PERFMON (idempotent; needs scoped sudo setcap).
grant_btop_intel_gpu_perf

log "Done. Restart btop to see the Intel Iris Xe GPU box."
