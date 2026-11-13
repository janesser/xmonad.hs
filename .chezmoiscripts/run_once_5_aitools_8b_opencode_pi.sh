#!/bin/bash
# run_once_5_aitools_8b_opencode_pi.sh
#
# Install opencode on raspberrypi, a Raspberry Pi 400 without a GPU — it cannot
# serve a model itself, so this is a **client install**: the standalone ARM64 CLI
# binary (no Node.js/npm required) lands at ~/.local/bin/opencode and talks to the
# opencode server on cyberkleiber, the box that owns both V100 backends:
#
#   ~/.local/bin/opencode                  # installed by this script
#   opencode --server http://cyberkleiber:49374   # connect to the AI server
#
# The web UI on cyberkleiber (opencode-web.service, :49374, bound to every
# interface) runs with both V100 backends attached (:8081/:8082), so every model
# call from the Pi still uses Sage-A1.5 Q4_K_M on a GPU — the Pi is only a
# terminal. A systemd unit (etc/systemd/system/opencode-pi.service) makes it an
# always-on entry point on the LAN, so other boxes can connect to it.
#
# Sibling of .chezmoiscripts/run_once_5_aitools_8opencode.sh (cyberkleiber).

set -euo pipefail

OPENCODE_BIN="${HOME}/.local/bin/opencode"
OPENCODE_URL="https://opencode.ai/update/api/latest/cli/opencode/opencode-linux-arm64.tar.gz"
SERVER="http://cyberkleiber:49374"

# --- 1. host gate (raspberrypi only — no GPU, no model) ---------------------------
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "⏭️  $(hostname) is not raspberrypi — opencode belongs on the AI server."
    exit 0
fi

# --- 2. arch gate (standalone binary needs arm64) ------------------------------------
if [ "$(uname -m)" != "aarch64" ]; then
    echo "⏭️  $(uname -m) is not arm64 — the standalone opencode binary doesn't fit."
    exit 0
fi

# --- 3. install the standalone ARM64 CLI binary --------------------------------------
if [ -x "${OPENCODE_BIN}" ]; then
    echo "✅ opencode already installed at ${OPENCODE_BIN} ($(opencode --version 2>/dev/null | head -1))"
else
    echo "📦 installing opencode (standalone Linux arm64) ..."
    mkdir -p "${HOME}/.local/bin"
    if ! curl -fsSL --max-time 120 "${OPENCODE_URL}" -o /tmp/opencode-linux-arm64.tar.gz; then
        echo "⚠️  download failed — install by hand:"
        echo "     curl -fsSL ${OPENCODE_URL} -o opencode-linux-arm64.tar.gz"
        echo "     tar xzf opencode-linux-arm64.tar.gz -C ~/.local/bin/ opencode"
        exit 1
    fi
    if ! tar xzf /tmp/opencode-linux-arm64.tar.gz -C "${HOME}/.local/bin/"; then
        rm -f /tmp/opencode-linux-arm64.tar.gz
        echo "⚠️  extraction failed — install by hand:"
        echo "     curl -fsSL ${OPENCODE_URL} -o opencode-linux-arm64.tar.gz"
        echo "     tar xzf opencode-linux-arm64.tar.gz -C ~/.local/bin/ opencode"
        exit 1
    fi
    rm -f /tmp/opencode-linux-arm64.tar.gz
    chmod +x "${OPENCODE_BIN}"
    echo "✅ opencode installed at ${OPENCODE_BIN} ($(opencode --version 2>/dev/null | head -1))"
fi

# --- 4. install the systemd unit ------------------------------------------------------
UNIT=opencode-pi.service
UNIT_SRC="${CHEZMOI_SOURCE_DIR:-.}/etc/systemd/system/${UNIT}"
UNIT_DEST="/etc/systemd/system/${UNIT}"
if [ -f "${UNIT_DEST}" ] && cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
    echo "✅ ${UNIT_DEST} already installed and up to date."
else
    # `install` is a constructive command in the scoped NOPASSWD sudoers drop-in.
    sudo install -o root -g root -m 0644 "${UNIT_SRC}" "${UNIT_DEST}"
    echo "✅ installed ${UNIT_DEST}"
fi
sudo systemctl daemon-reload
sudo systemctl enable "${UNIT}"

# --- 5. start when not already serving -----------------------------------------------
# Guard on the healthz probe (not is-active): never restart a live server.
if code=$(curl -sS --connect-timeout 5 -o /dev/null -w "%{http_code}" "http://127.0.0.1:49374/" 2>/dev/null || echo 000); then
    if [ "${code}" != "000" ]; then
        echo "✅ opencode already serving on :49374 (HTTP ${code}) — left running."
    else
        sudo systemctl start "${UNIT}"
        echo "✅ ${UNIT} started."
    fi
else
    echo "⚠️  could not check :49374 — starting ${UNIT}."
    sudo systemctl start "${UNIT}"
    echo "✅ ${UNIT} started."
fi

# --- 6. sanity: the box can reach cyberkleiber ----------------------------------------
if code=$(curl -sS --connect-timeout 5 -o /dev/null -w "%{http_code}" "${SERVER}/" 2>/dev/null || echo 000); then
    if [ "${code}" != "000" ]; then
        echo "✅ $(hostname) can reach the opencode server on cyberkleiber (:49374, HTTP ${code})."
    else
        echo "⚠️  $(hostname) cannot reach the opencode server on cyberkleiber — check the LAN."
    fi
else
    echo "⚠️  could not check connectivity to the opencode server."
fi

# --- next steps --------------------------------------------------------------------
echo ""
echo "To use it:"
echo "   opencode --server http://cyberkleiber:49374"
echo "   # or in a browser: open the pairing link from 'opencode pair'"
echo ""
echo "The server password is printed by 'systemctl log opencode-web.service' on"
echo "cyberkleiber; persist it for your session as OPENCODE_PASSWORD so each"
echo "connect does not ask for it again."
