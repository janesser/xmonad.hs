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
#   * Writes localai.service (systemd, User=jan) to /etc/systemd/system and
#     enables it at boot. The SYCL backend is fetched on first model load.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

LOCALAI_VERSION="v4.10.0"
BIN="$HOME/.local/bin/localai"
MODELS_DIR="$HOME/.local/share/localai/models"
BACKENDS_DIR="$HOME/.local/share/localai/backends"
UNIT_NAME="localai.service"
UNIT_SRC="$SRC_DIR/etc/systemd/system/$UNIT_NAME"
UNIT_DEST="/etc/systemd/system/$UNIT_NAME"

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
# installs the front + boot-enable — no model loads yet.
mkdir -p "$MODELS_DIR" "$BACKENDS_DIR"
echo "✅ LocalAI data dirs ready: $MODELS_DIR, $BACKENDS_DIR"

# --- 1.5. install the host Intel compute-runtime (Level Zero driver) --------
# LocalAI's bundled SYCL driver predates the kernel 7.0 i915 ABI and returns
# "no device"; the host libze-intel-gpu1 (v26.05, Ubuntu universe) enumerates
# the DG1. Installed via apt (scoped NOPASSWD) before the unit is copied, since
# the unit's ZIC_ENABLE_ALT_DRIVERS points at the path apt installs to.
if dpkg -l libze-intel-gpu1 2>/dev/null | grep -q '^ii'; then
    echo "✅ libze-intel-gpu1 already installed."
else
    sudo apt install -y libze-intel-gpu1
    echo "✅ Installed libze-intel-gpu1 (host Intel Level Zero driver)."
fi

# --- 2. install + enable the localai service -------------------------------
if [ ! -f "$UNIT_SRC" ]; then
    echo "⚠️  Unit source not found: $UNIT_SRC"
    exit 1
fi
if [ -f "$UNIT_DEST" ] && cmp -s "$UNIT_SRC" "$UNIT_DEST"; then
    echo "✅ $UNIT_DEST already installed and up to date."
else
    if [ -f "$UNIT_DEST" ]; then sudo rm -f "$UNIT_DEST"; fi
    sudo install -o root -g root -m 0644 "$UNIT_SRC" "$UNIT_DEST"
    echo "✅ Installed $UNIT_DEST"
fi
sudo systemctl enable "$UNIT_NAME" >/dev/null 2>&1
sudo systemctl daemon-reload
echo "✅ LocalAI deployed (staged: front enabled at boot, Olla stays live as the active path)."
