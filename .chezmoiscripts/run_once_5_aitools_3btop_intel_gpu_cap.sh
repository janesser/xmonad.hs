#!/bin/bash
# run_once_5_aitools_3btop_intel_gpu_cap.sh
#
# Make btop surface Intel Iris Xe GPU usage (and, in general, any i915-based
# integrated GPU). Two things are required, both done here:
#
#   1. A patched btop binary. btop 1.4.6 hardcodes the i915 perf-PMU sysfs path
#      to /sys/devices/i915/events, but recent kernels instantiate the device by
#      its PCI address (e.g. /sys/devices/i915_0000_2f_00.0/events). btop then
#      fails to discover the engines and the Iris Xe box silently vanishes. The
#      pinned patch below resolves the concrete i915_* device by scanning
#      /sys/devices, so discover_engines() + pmu_init() both succeed. Built from
#      source (tag v1.4.6) and installed over /usr/bin/btop.
#
#   2. CAP_PERFMON on /usr/bin/btop. btop attaches to the i915 perf PMU via
#      perf_event_open(); a normal user gets EPERM unless it holds CAP_PERFMON
#      (kernel.perf_event_paranoid >= 4 blocks it). No external package is
#      needed — just the capability on the binary.
#
# SAFEGUARD "where required":
#   Only run this on a box that actually has an Intel GPU (has_intel_gpu()).
#   On ARM / NVIDIA-only / headless machines we touch nothing, so CAP_PERFMON
#   is never handed out unnecessarily. GPU detection is centralised in
#   ~/.local/share/gpu.func (sourced below) — the same never-false-negative
#   lspci presence test used by the llama.cpp build script.
#
# Root is only ever used for commands allowed by the scoped chezmoi-pi sudoers
# drop-in (see AGENTS.md): `install` (to place the binary) and
# `/usr/sbin/setcap cap_perfmon+ep /usr/bin/btop` (exact match). NOTE: setcap
# lives in /usr/sbin, not /usr/bin, so the sudoers rule must name the resolved
# path /usr/sbin/setcap — sudo matches on the resolved path.
#
# Build steps (git/cmake/make) run as the user (no sudo needed); only the
# final install + setcap use scoped sudo. This script is run_once, so the
# (slow) build happens on the first cz update; subsequent updates are no-ops.

set -o pipefail

BTOP_DIR="$HOME/projs/btop"
BTOP_TAG="v1.4.6"
BTOP_PATCH="$BTOP_DIR/0001-i915-discovery.patch"

# Detection + the capability helper live in gpu.func (rendered to
# ~/.local/share by chezmoi before run scripts execute).
source "$HOME/.local/share/gpu.func"

log() { echo "$(basename "$0"): $*"; }

# ensure_btop -> build the patched btop from pinned source and install it over
# the system binary. Returns non-zero on failure (logged, never fatal, so a
# failed build during cz update leaves the working system btop in place for a
# retry on the next update).
ensure_btop() {
    if [ ! -x "$BTOP_DIR/build/btop" ]; then
        # Clone the pinned tag once (idempotent). A full clone is preferred over
        # the shallow one so MR/inspection works locally; either way we only
        # clone if there is no .git yet, so cz update needs no network when the
        # clone already exists.
        if [ ! -d "$BTOP_DIR/.git" ]; then
            log "Cloning btop $BTOP_TAG -> $BTOP_DIR"
            if ! git clone --branch "$BTOP_TAG" \
                    https://github.com/aristocratos/btop.git "$BTOP_DIR"; then
                log "ERROR: could not clone btop $BTOP_TAG (check network) and re-run."
                return 1
            fi
        fi

        # Apply the pinned i915 discovery fix.
        cat > "$BTOP_PATCH" <<'BTOP_PATCH'
Subject: [PATCH] linux: detect i915 GPU on kernels that name the perf device by PCI address

Since recent kernels the i915 perf PMU is no longer exposed at the
stable /sys/devices/i915/events path; the device is instantiated by its
PCI address (e.g. /sys/devices/i915_0000_2f_00.0/events). btop hardcodes
the driver name, so discover_engines() and pmu_init() both fail to find
the i915 events dir and perf event source, and integrated Iris Xe /
Intel Arc GPUs silently disappear from the gpu box.

Resolve the concrete i915_* device by scanning /sys/devices for an
entry exposing an events subdir, and propagate that instance name into
engines->device so pmu_init() can locate the perf event source type.

---
--- a/src/linux/intel_gpu_top/intel_gpu_top.c
+++ b/src/linux/intel_gpu_top/intel_gpu_top.c
@@ -282,13 +282,40 @@
 struct engines *discover_engines(const char *device)
 {
 	char sysfs_root[PATH_MAX];
+	char device_name[64];
 	struct engines *engines;
 	struct dirent *dent;
 	int ret = 0;
-	DIR *d;
+	DIR *d, *scan;
 
+	//* The i915 perf PMU used to be exposed as
+	//* /sys/devices/<driver>/events. Kernels that identify the device by
+	//* its PCI address expose it as /sys/devices/i915_<b:d.f>/events
+	//* instead, so the bare <driver> path is absent. Resolve the concrete
+	//* i915_* device by scanning /sys/devices for an entry exposing an
+	//* events subdir, and remember its name (pmu_init needs it to find the
+	//* perf event source type).
 	snprintf(sysfs_root, sizeof(sysfs_root),
 		 "/sys/devices/%s/events", device);
+	snprintf(device_name, sizeof(device_name), "%s", device);
+	if ((scan = opendir("/sys/devices")) != NULL) {
+		struct dirent *ed;
+		while ((ed = readdir(scan)) != NULL) {
+			if (strncmp(ed->d_name, "i915", 4) != 0)
+				continue;
+			char cand[PATH_MAX];
+			snprintf(cand, sizeof(cand),
+				"/sys/devices/%s/events", ed->d_name);
+			if (opendir(cand) != NULL) {
+				snprintf(sysfs_root, sizeof(sysfs_root),
+					"%s", cand);
+				snprintf(device_name, sizeof(device_name),
+					"%s", ed->d_name);
+				break;
+			}
+		}
+		closedir(scan);
+	}
 
 	engines = malloc(sizeof(struct engines));
 	if (!engines)
@@ -297,7 +324,10 @@
 	memset(engines, 0, sizeof(*engines));
 
 	engines->num_engines = 0;
-	engines->device = device;
+	//* engines->device drives the perf-event-source lookup in pmu_init
+	//* (/sys/bus/event_source/devices/<device>/type), so it must carry the
+	//* resolved instance name rather than just "i915".
+	engines->device = strdup(device_name);
 	engines->discrete = !is_igpu(device);
 
 	d = opendir(sysfs_root);
@@ -397,6 +427,8 @@
 	return engines;
 
 err:
+	if (engines->device)
+		free(engines->device);
 	free(engines);
 
 	return NULL;
@@ -431,6 +463,8 @@
 
 	closedir(engines->root);
 
+	if (engines->device)
+		free(engines->device);
 	free(engines->class);
 	free(engines);
 }

BTOP_PATCH
        # Apply the i915 discovery fix. Reset the patched file first so the
        # apply is deterministic even if a previous run left the patch applied
        # (idempotent across re-clones / build deletions).
        if ! ( cd "$BTOP_DIR" \
                && git checkout -- src/linux/intel_gpu_top/intel_gpu_top.c 2>/dev/null \
                && git apply "$BTOP_PATCH" ); then
            log "ERROR: could not apply the i915 discovery patch to btop $BTOP_TAG"
            log "     (upstream layout may have changed at that tag)."
            return 1
        fi

        # Build as the user (cmake/make need no root).
        local ncpus
        ncpus="$(grep -c '^processor' /proc/cpuinfo 2>/dev/null || echo 4)"
        log "Building btop $BTOP_TAG ($ncpus cores) ..."
        if ! ( cd "$BTOP_DIR" && cmake -B build . \
                    -DCMAKE_BUILD_TYPE=Release \
                && cmake --build build -j "$ncpus" ); then
            log "ERROR: btop build failed — fix the above and re-run cz update."
            return 1
        fi
    else
        log "btop $BTOP_TAG already built at $BTOP_DIR/build."
    fi

    # Install over the system binary (scoped sudo 'install').
    if ! sudo install -m755 "$BTOP_DIR/build/btop" /usr/bin/btop; then
        log "ERROR: could not install patched btop to /usr/bin/btop."
        return 1
    fi
    log "installed patched btop $BTOP_TAG to /usr/bin/btop"
    return 0
}

if ! has_intel_gpu; then
    log "No Intel GPU detected, skipping btop patch + cap_perfmon grant..."
    exit 0
fi

ensure_btop || log "btop patch/install was not applied — see errors above; re-run cz update."

# Grant /usr/bin/btop CAP_PERFMON (idempotent; needs scoped sudo setcap).
grant_btop_intel_gpu_perf

log "Done. Restart btop to see the Intel Iris Xe GPU box."
