#!/bin/bash
# run_once_5_aitools_8opencode.sh
#
# Install opencode (the OpenCode AI coding agent) on cyberkleiber, the box that
# owns both GPU backends, and make it usable:
#
#   ~/.local/share/mise/installs/node/latest/bin/opencode  # @opencode/cli@latest
#   /etc/systemd/system/opencode-web.service             # systemd unit (source)
#   ~/.config/opencode/opencode.json                      # rendered by chezmoi
#
# The web UI runs under the systemd unit on every interface at port 49374:
#
#   http://cyberkleiber:49374          # browser web UI (via opencode pair)
#   opencode --server :49374           # TUI connected to the same server
#
# The provider config is rendered by chezmoi from
# dot_config/opencode/opencode.json onto this box:
#
#   llama-cuda-card1/sage-a1.5     -> :8081 (V100 SXM2 CUDA0, llama-cuda.service)
#   llama-cuda-card2/sage-a1.5     -> :8082 (PCIe V100 card 2, llama-little-brother.service)
#
# Both cards serve InternScience/Sage-A1.5-Q4_K_M-GGUF:Q4_K_M via the OpenAI
# compatible API at /v1/models and /v1/chat/completions. The provider key (left
# of `/`) is an opencode-side label; the model ID (right of `/`) is sent to the
# server, which serves the loaded model regardless of the name it receives.
#
# cyberkleiber ONLY — both cards live on this box. On any other host this script
# logs and exits 0.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in (see
# AGENTS.md): install, systemctl. `npm i -g` writes into the mise node dir and
# needs no sudo. The unit file is installed from etc/systemd/system/ (the same
# pattern as the llama-cuda / pi-memory-hub units).

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
UNIT=opencode-web.service
UNIT_SRC="${SRC_DIR}/etc/systemd/system/${UNIT}"
UNIT_DEST="/etc/systemd/system/${UNIT}"

# --- 1. host gate (cyberkleiber only — both cards live here) -------------------
if [ "$(hostname)" != "cyberkleiber" ]; then
    echo "⏭️  $(hostname) is not cyberkleiber — opencode's local backends are only on the AI server."
    exit 0
fi

# --- 2. node via mise (the npm used to install @opencode/cli) -------------------
if ! command -v npm >/dev/null; then
    source "$SRC_DIR/commons/mise_assert-node.sh"
else
    echo "✅ node is available: $(node --version)"
fi

# --- 3. install opencode ---------------------------------------------------------
OPENCODE_BIN="$(command -v opencode 2>/dev/null || echo "")"
if [ -x "${OPENCODE_BIN}" ]; then
    echo "✅ opencode already installed at ${OPENCODE_BIN} ($(opencode --version 2>/dev/null | head -1))"
else
    echo "📦 installing @opencode/cli@latest via npm..."
    if ! npm i -g @opencode/cli@latest >/dev/null 2>&1; then
        echo "⚠️  npm install failed — check disk space (df /home) and run the install by hand:"
        echo "     npm i -g @opencode/cli@latest"
        exit 1
    fi
    OPENCODE_BIN="$(command -v opencode)"
    echo "✅ opencode installed at ${OPENCODE_BIN} ($(opencode --version 2>/dev/null | head -1))"
fi

# --- 4. install the systemd unit -------------------------------------------------
if [ -f "${UNIT_DEST}" ] && cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
    echo "✅ ${UNIT_DEST} already installed and up to date."
else
    sudo install -o root -g root -m 0644 "${UNIT_SRC}" "${UNIT_DEST}"
    echo "✅ installed ${UNIT_DEST}"
fi
sudo systemctl daemon-reload
sudo systemctl enable "${UNIT}"

# --- 5. start when not already serving ------------------------------------------
# Guard on the healthz probe (not is-active): never restart a live server.
if code=$(curl -sS --connect-timeout 5 -o /dev/null -w "%{http_code}" "http://127.0.0.1:49374/" 2>/dev/null || echo 000); then
    if [ "${code}" != "000" ]; then
        echo "✅ opencode web UI already serving on :49374 (HTTP ${code}) — left running."
    else
        sudo systemctl start "${UNIT}"
        echo "✅ ${UNIT} started."
    fi
else
    echo "⚠️  could not check :49374 — starting ${UNIT}."
    sudo systemctl start "${UNIT}"
    echo "✅ ${UNIT} started."
fi

# --- 6. verify the backends opencode will talk to -------------------------------
for PORT in 8081 8082; do
    if curl -fsS --connect-timeout 5 "http://127.0.0.1:${PORT}/v1/models" >/dev/null 2>&1; then
        echo "✅ llama.cpp serving /v1/models on :${PORT}"
    else
        echo "⚠️  no llama.cpp backend on :${PORT} — opencode falls back to Zen or a remote model."
    fi
done

echo "LAN access: http://cyberkleiber:49374 (needs 'opencode pair' session via ssh -L 49374:127.0.0.1:49374 cyberkleiber)"
echo "Verify the default: opencode run --model llama-cuda-card1/sage-a1.5 'hello'"
