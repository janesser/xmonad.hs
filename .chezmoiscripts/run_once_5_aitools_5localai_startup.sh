#!/bin/bash
# run_once_5_aitools_5localai_startup.sh
#
# Install LocalAI (mudler/LocalAI) as the chezmoi-managed, boot-persistent
# OpenAI-compatible front + model orchestrator for the V100 (CUDA). Per the
# §11 design in discoveries/ai-server.md, LocalAI supersedes the Olla topology
# (run_once_5_aitools_3olla_startup.sh) during a STAGED cutover:
#
#   * Olla + llama-cuda stay the live path during staging.
#   * LocalAI is installed + enabled at boot, but its ornith.gguf model is
#     DISABLED, so it does NOT load on boot and does not contend for V100 VRAM
#     with the live backend. At cutover: enable ornith + flip pi to LocalAI.
#
#   * Downloads the precompiled LocalAI binary (no docker dependency — matches
#     the box's unit+binary pattern) to ~/.local/bin/localai.
#   * Writes localai.service (systemd, User=jan) to /etc/systemd/system and
#     enables it at boot.
#   * Registers ornith.gguf (llama-cpp, NVIDIA backend) in the models dir.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"

LOCALAI_VERSION="v4.10.0"
BIN="$HOME/.local/bin/localai"
MODELS_DIR="$HOME/.local/share/localai/models"
BACKENDS_DIR="$HOME/.local/share/localai/backends"
ORNITH="$HOME/.cache/huggingface/hub/ornith.gguf"
LINK="$MODELS_DIR/ornith.gguf"
YAML="$MODELS_DIR/ornith.gguf.yaml"
UNIT_NAME="localai.service"
UNIT_SRC="$SRC_DIR/etc/systemd/system/$UNIT_NAME"
UNIT_DEST="/etc/systemd/system/$UNIT_NAME"

# GPU-presence helper (has_nvidia). Sourced, not executed.
source "$HOME/.local/share/gpu.func"
if ! has_nvidia; then
    echo "$(basename "$0"): no NVIDIA GPU — skipping LocalAI install."
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
mkdir -p "$MODELS_DIR" "$BACKENDS_DIR"

# --- 2. register the ornith.gguf model (DISABLED during staging) ------------
# LocalAI v4 loads models from --models-path; parameters.model is RELATIVE to
# it, so symlink the model in and reference it by filename. The model is
# DISABLED so it does NOT load on boot and contend for V100 VRAM with the live
# Olla/llama-cuda backend. At cutover: flip enabled: false -> true + flip pi.
if [ ! -e "$LINK" ]; then
    ln -sf "$ORNITH" "$LINK"
    echo "✅ Linked $ORNITH -> $LINK"
fi
cat > "$YAML" <<EOF
name: ornith
backend: llama-cpp
parameters:
  model: ornith.gguf
  context_size: 8192
  threads: 4
  gpu_layers: 0
enabled: false
EOF
echo "✅ Registered ornith.gguf model in $MODELS_DIR (disabled — staged)"

# --- 3. install + enable the localai service -------------------------------
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
echo "✅ $UNIT_NAME enabled at boot (ornith disabled — staged; Olla stays live)"

sudo systemctl daemon-reload
echo "✅ LocalAI deployed (staged: ornith disabled, Olla live)."
