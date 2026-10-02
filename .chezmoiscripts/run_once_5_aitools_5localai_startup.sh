#!/bin/bash
# run_once_5_aitools_5localai_startup.sh
#
# Install LocalAI (mudler/LocalAI) as the chezmoi-managed, boot-persistent
# OpenAI-compatible front + model orchestrator. Per the §11 design in
# discoveries/ai-server.md, LocalAI supersedes the Olla topology
# (run_once_5_aitools_3olla_startup.sh) during a STAGED cutover:
#
#   * Olla + llama-cuda stay the live path during staging.
#   * LocalAI is installed + enabled at boot, pinned to the Intel SYCL backend
#     (Iris Xe) so a model can be served with NO V100 VRAM contention. The
#     llama-cuda/cutover (V100, CUDA) is the later end-state.
#
#   * Downloads the precompiled LocalAI binary (no docker dependency — matches
#     the box's unit+binary pattern) to ~/.local/bin/localai.
#   * Installs the host Intel compute-runtime (libze-intel-gpu1) so the SYCL
#     backend works under kernel 7.0 (LocalAI's bundled driver predates the
#     i915 ABI and returns "no device").
#   * Deploys no --system unit. The live systemd unit is a --user service
#     (boot-persistent via linger). The disabled --system fallback unit that
#     used to be written here was retired 2026-10-03 (item 3).
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

LOCALAI_VERSION="v4.10.0"
BIN="$HOME/.local/bin/localai"
MODELS_DIR="$HOME/.local/share/localai/models"
BACKENDS_DIR="$HOME/.local/share/localai/backends"


# GPU-presence helper. Sourced, not executed. The V100 box has both NVIDIA and
# an Intel Iris Xe, so the guard is any-GPU; nvidia-smi is a driver-backed
# fallback for a transient lspci glitch (the install must not skip just because
# lspci hiccupped — the box demonstrably has a V100).
source "$HOME/.local/share/gpu.func"
if ! (has_nvidia || has_intel_gpu || command -v nvidia-smi >/dev/null 2>&1); then
    echo "$(basename "$0"): no GPU found — skipping LocalAI install."
    exit 0
fi

# --- 0. download + install the precompiled LocalAI binary -------------------
# Precompiled binary (no docker daemon dependency). Network at install;
# idempotent — skip when the pinned version is already present.
if [ -x "$BIN" ] && "$BIN" --version 2>/dev/null | grep -q "$LOCALAI_VERSION"; then
    echo "✅ LocalAI $LOCALAI_VERSION already installed at $BIN"
else
    case "$(uname -m)" in
        x86_64) LA_ARCH="linux-amd64" ;;
        aarch64) LA_ARCH="linux-arm64" ;;
        *) echo "ERROR: unsupported arch $(uname -m)" >&2; exit 1 ;;
    esac
    URL="https://github.com/mudler/LocalAI/releases/download/${LOCALAI_VERSION}/local-ai-${LOCALAI_VERSION}-${LA_ARCH}"
    TMP="$(mktemp)"
    echo "Downloading LocalAI $LOCALAI_VERSION ($LA_ARCH)..."
    # Retry a few times — the GitHub release host can be flaky; a failed
    # download would otherwise fail this run_once (set -e).
    for attempt in 1 2 3; do
        if curl -fsSL --retry 2 --retry-delay 5 -o "$TMP" "$URL"; then break; fi
        echo "  download attempt $attempt failed; retrying..." >&2
    done
    [ -s "$TMP" ] || { echo "ERROR: LocalAI download failed" >&2; exit 1; }
    install -m 0755 "$TMP" "$BIN"
    rm -f "$TMP"
    echo "✅ Installed LocalAI $LOCALAI_VERSION -> $BIN"
fi

# --- 1. persistent data dirs (models + backends) ----------------------------
# These are the persistent LocalAI data dirs (survive reboot). Model configs
# live here and are added at their respective steps (SYCL smoke test now;
# ornith on the V100 at the later llama-cuda cutover). This staged deploy only
# installs the front + driver — no model loads, no unit is deployed.
mkdir -p "$MODELS_DIR" "$BACKENDS_DIR"
echo "✅ LocalAI data dirs ready: $MODELS_DIR, $BACKENDS_DIR"

# --- 1.5. install the host Intel compute-runtime (Level Zero driver) --------
# LocalAI's bundled SYCL driver predates the kernel 7.0 i915 ABI and returns
# "no device"; the host libze-intel-gpu1 (v26.05, Ubuntu universe) enumerates
# the DG1. Installed via apt (scoped NOPASSWD) before the SYCL backend first
# loads, so the host Level Zero driver is present for the --user unit's SYCL
# backend (which points at the same path via ZIC_ENABLE_ALT_DRIVERS).
if dpkg -l libze-intel-gpu1 2>/dev/null | grep -q '^ii'; then
    echo "✅ libze-intel-gpu1 already installed."
else
    sudo apt install -y libze-intel-gpu1
    echo "✅ Installed libze-intel-gpu1 (host Intel Level Zero driver)."
fi

# --- 2. --system fallback unit: RETIRED -----------------------------------
# A disabled --system localai.service used to be installed here as a SYCL
# fallback ("until the --user unit serves a SYCL model on :8080"). That
# condition is met: the --user unit serves SYCL on :8080 and is boot-persistent
# (Linger=yes). The stale --system unit was retired 2026-10-03 (see
# discoveries/ai-server.md, handover item 3). No unit is deployed by this
# script; the --user unit is the sole live path.
echo "✅ LocalAI deployed: binary + host Intel driver installed. No --system unit (retired 2026-10-03, item 3)."
