#!/usr/bin/env bash
# Load the open-source nouveau KMS driver for the GeForce GT 730 (GK208B, Maxwell).
# Dependency: mxm_wmi exports the mxm_wmi_* symbols nouveau needs, so it must
# be inserted first.  insmod-by-path is used deliberately: `modprobe nouveau`
# mis-resolves to "off" on this host (see journal).
# Part of the cyberkleiber dual-driver setup: GT730 -> nouveau, V100 -> nvidia.
set -euo pipefail

ins() {
  local modfile="$1" base
  base="$(basename "$1" .ko.zst)"          # mxm-wmi.ko.zst -> mxm-wmi
  base="${base//-/_}"                        # mxm-wmi -> mxm_wmi (lsmod spelling)
  if lsmod | grep -qw "$base"; then
    echo "$base already loaded, skipping"
    return 0
  fi
  /usr/sbin/insmod "/lib/modules/$(uname -r)/$modfile"
}

# mxm_wmi first, then nouveau.  nouveau auto-binds the free GT 730; the V100 is
# already owned by nvidia (this service starts After it), so it is left untouched.
ins kernel/drivers/platform/x86/mxm-wmi.ko.zst
ins kernel/drivers/gpu/drm/nouveau/nouveau.ko.zst
