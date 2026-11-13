#!/bin/bash
# run_once_5_aitools_8opencode.sh
#
# United setup for jan's opencode instances — one userspace unit, shared by
# every box that has opencode on PATH:
#
#   ~/.config/systemd/user/opencode.service -> the united userspace unit
#                                             (from dot_config/systemd/user/)
#   opencode on PATH                       -> the binary to run; it lands there
#                                             either way:
#       /home/jan/.local/share/mise/installs/node/latest/bin/opencode
#                                            npm/mise install (the AI server)
#       /home/jan/.local/share/mise/shims/opencode
#                                            mise shim -> /usr/bin/mise (flowerpower)
#       /home/jan/.local/bin/opencode
#                                            standalone arm64 (raspberrypi)
#
# The web UI runs under the united userspace unit on every interface at port
# 49374:
#
#   http://<box>:49374          # browser web UI (via 'opencode pair')
#   opencode --server :49374    # TUI connected to the same server
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
# This script runs on every box: opencode must be on PATH — install it if not.
# Then enable + start the userspace unit and verify the backends opencode will
# talk to. On any host where opencode cannot be installed, this is a no-op
# (the unit stays disabled). The obsolete system units that used to run here are
# disabled by this script during the transition (their sources are gone from the repo).
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in (see
# AGENTS.md): install, systemctl. `npm i -g` writes into the mise node dir and
# needs no sudo. The unit is a chezmoi target under dot_config/systemd/user/ —
# it renders into ~/.config/systemd/user/ and only needs to be enabled here.
#
# Subsumes .chezmoiscripts/run_once_5_aitools_8b_opencode_pi.sh (raspberrypi):
# the unit is shared, so there is no separate Pi install script anymore.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
UNIT=opencode.service
UNIT_SRC="${SRC_DIR}/dot_config/systemd/user/${UNIT}"
UNIT_DEST="${HOME}/.config/systemd/user/${UNIT}"
UI_PORT=49374
OPENCODE_URL="https://opencode.ai/update/api/latest/cli/opencode/opencode-linux-arm64.tar.gz"

# --- 1. opencode on PATH (install if missing) ------------------------------------
# The unit calls `opencode` from PATH, so opencode must land on PATH here or
# nowhere. If neither install path works, this box is not an opencode host:
# nothing is done and the apply keeps running.
if ! command -v opencode >/dev/null; then
    echo "📦 opencode not on PATH — installing it ..."
    if [ "$(uname -m)" = "aarch64" ]; then
        # No GPU + no node: the standalone arm64 CLI binary (raspberrypi).
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
        chmod +x "${HOME}/.local/bin/opencode"
        echo "✅ opencode installed at ${HOME}/.local/bin/opencode ($(opencode --version 2>/dev/null | head -1))"
    else
        # npm/mise install (all other boxes).
        if ! command -v npm >/dev/null; then
            source "$SRC_DIR/commons/mise_assert-node.sh"
        fi
        echo "📦 installing @opencode/cli@latest via npm..."
        if ! npm i -g @opencode/cli@latest >/dev/null 2>&1; then
            echo "⚠️  npm install failed — check disk space (df /home) and run the install by hand:"
            echo "     npm i -g @opencode/cli@latest"
        else
            echo "✅ opencode installed: $(command -v opencode) ($(opencode --version 2>/dev/null | head -1))"
        fi
    fi
fi
if ! command -v opencode >/dev/null; then
    echo "⚠️  opencode could not be installed on $(hostname) — this box is not an opencode host, skipping."
    exit 0
fi
echo "✅ opencode on PATH: $(command -v opencode) ($(opencode --version 2>/dev/null | head -1))"

# --- 2. linger so the userspace unit starts at boot -------------------------------
if [ "$(loginctl show-user jan --no-ask-password -p Linger --value 2>/dev/null || echo '')" != "yes" ]; then
    if loginctl enable-linger jan 2>&1; then
        echo "✅ linger enabled for jan — userspace units now start at boot"
    else
        echo "⚠️  could not enable linger for jan (userspace units need it to start at boot)"
        echo "     run: loginctl enable-linger jan"
    fi
fi

# --- 3. the united userspace unit (already rendered by chezmoi) ---------------------
if [ ! -f "${UNIT_DEST}" ]; then
    echo "=> ERROR: ${UNIT} missing at ${UNIT_DEST} — not rendered by chezmoi?"
    exit 1
fi
systemctl --user enable "${UNIT}" --now 2>/dev/null || {
    echo "=> ERROR: could not enable + start ${UNIT}"
    exit 1
}
echo "✅ ${UNIT} enabled and started"

# --- 3b. disable the obsolete system units (transitional) --------------------------
# Both boxes carried a system unit (`opencode-web.service` / `opencode-pi.service`)
# at :49374 before this refactor. If they are still enabled, they bind that port at
# boot and the new userspace unit cannot start — the Pi's old opencode-pi.service did
# 460 restarts trying to bind it. `systemctl` is in the scoped NOPASSWD sudoers
# drop-in; this only disables what used to be installed (its source is gone from the
# repo now that ${UNIT} is the unit).
for unit in opencode-web.service opencode-pi.service; do
    if [ -f "/etc/systemd/system/${unit}" ]; then
        if sudo systemctl disable --now "${unit}" >/dev/null 2>&1; then
            echo "✅ disabled + stopped /etc/systemd/system/${unit} (replaced by ${UNIT})"
        else
            echo "⚠️  could not disable /etc/systemd/system/${unit} — remove it by hand if it stays enabled"
        fi
    else
        echo "=> /etc/systemd/system/${unit} is not installed (nothing to do)"
    fi
done

# --- 4. start when not already serving -------------------------------------------
# The unit's PATH includes the node/mise bin dir, the mise shims and ~/.local/bin,
# so `opencode` resolves to whichever install this box carries:
#   /home/jan/.local/share/mise/installs/node/latest/bin/opencode      (npm)
#   /home/jan/.local/share/mise/shims/opencode                         (mise shim)
#   /home/jan/.local/bin/opencode                                      (standalone arm64)
# Guard on the healthz probe (not is-active): never restart a live server.
if code=$(curl -sS --connect-timeout 5 -o /dev/null -w "%{http_code}" "http://127.0.0.1:${UI_PORT}/" 2>/dev/null || echo 000); then
    if [ "${code}" != "000" ]; then
        echo "✅ opencode web UI already serving on :${UI_PORT} (HTTP ${code}) — left running."
    else
        # Nothing is serving: a stale opencode process may still hold :${UI_PORT}.
        # Reap it before starting — the old Pi unit did 460 restarts trying to bind.
        for p in $(ss -ltnp 2>/dev/null | grep ":${UI_PORT} " | grep -oE 'pid=[0-9]+' | sed 's/pid=//'); do
            cmdline=$(tr '\0' ' ' < /proc/$p/cmdline 2>/dev/null)
            case "$cmdline" in *"opencode serve --hostname 0.0.0.0 --port ${UI_PORT}"*) : ;; *) kill -TERM "$p" 2>/dev/null && echo "reaped stale opencode $p holding :${UI_PORT}";; esac
        done
        sleep 1
        systemctl --user start "${UNIT}"
        echo "✅ ${UNIT} started (nothing was serving on :${UI_PORT})."
    fi
else
    echo "⚠️  could not check :${UI_PORT} — starting ${UNIT}."
    systemctl --user start "${UNIT}"
    echo "✅ ${UNIT} started."
fi

# --- 5. verify the backends opencode will talk to ----------------------------------
for PORT in 8081 8082; do
    if curl -fsS --connect-timeout 5 "http://127.0.0.1:${PORT}/v1/models" >/dev/null 2>&1; then
        echo "✅ llama.cpp serving /v1/models on :${PORT}"
    else
        echo "⚠️  no llama.cpp backend on :${PORT} — opencode falls back to Zen or a remote model."
    fi
done

echo ""
echo "To use it:"
echo "   opencode --server http://<box>:49374        # TUI connected to the server"
echo "   opencode pair                               # one-time link for browser/app"
echo "   executable_opencode-pair.sh                 # shell calls for both instances"
