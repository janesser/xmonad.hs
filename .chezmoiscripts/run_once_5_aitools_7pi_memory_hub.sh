#!/bin/bash
# run_once_5_aitools_7pi_memory_hub.sh
#
# Deploy the pi-hermes-memory v2 shared memory hub (spine AD-1/AD-2/AD-14):
#
#   code checkout   ~/.local/share/pi-hermes-memory/hub-src   (jan's fork)
#   TLS material    ~/.local/share/pi-hermes-memory/hub/tls/  (local-only)
#   store + tokens  ~/.local/share/pi-hermes-memory/hub/      (local-only)
#   launcher        ~/.local/bin/pi-memory-hub-serve.sh
#   unit            /etc/systemd/system/pi-memory-hub.service (User=jan, boot)
#
# cyberkleiber ONLY — the hub is the single brain box (spine AD-1). On any
# other host this script logs and exits 0. The hub is a SEPARATE boot service
# from the AI backend (AD-2): own lifecycle, own port 8083, so llama-cuda
# handovers never darken shared memory.
#
# Secrets never touch the repo (AD-5/AD-14): the local CA + server cert and
# the token store are generated/placed under ~/.local/share/pi-hermes-memory/
# only — the unit and the launcher carry no tokens or keys.
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in
# (see the repo AGENTS.md).

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
UNIT=pi-memory-hub.service
UNIT_SRC="${SRC_DIR}/etc/systemd/system/${UNIT}"
UNIT_DEST="/etc/systemd/system/${UNIT}"
LAUNCHER_DEST="${HOME}/.local/bin/pi-memory-hub-serve.sh"
HUB_HOME="${HOME}/.local/share/pi-hermes-memory"
HUB_SRC="${HUB_HOME}/hub-src"
HUB_DATA="${HUB_HOME}/hub"
TLS_DIR="${HUB_DATA}/tls"
STAMP="${HUB_HOME}/hub_deployed_commit"
REPO_SSH="git@github.com:janesser/pi-hermes-memory.git"
REPO_HTTPS="https://github.com/janesser/pi-hermes-memory.git"
PORT=8083

# --- 1. host gate (cyberkleiber only, AD-1) -----------------------------------
if [ "$(hostname)" != "cyberkleiber" ]; then
    echo "⏭️  $(hostname) is not cyberkleiber — the memory hub is cyberkleiber-only (spine AD-1)."
    exit 0
fi

# --- 2. code checkout ----------------------------------------------------------
if [ ! -d "${HUB_SRC}/.git" ]; then
    echo "📦 cloning pi-hermes-memory (jan's fork) into ${HUB_SRC}"
    git clone "${REPO_SSH}" "${HUB_SRC}" 2>/dev/null || git clone "${REPO_HTTPS}" "${HUB_SRC}"
    ( cd "${HUB_SRC}" && npm ci --no-audit --no-fund --loglevel=error )
    git -C "${HUB_SRC}" rev-parse HEAD > "${STAMP}"
else
    # Refresh from the remote's main when reachable; keep the existing checkout
    # when the fetch fails (offline box, or the hub work is not pushed yet).
    if git -C "${HUB_SRC}" fetch origin main --quiet 2>/dev/null; then
        STAMPED="$(cat "${STAMP}" 2>/dev/null || echo "")"
        REMOTE="$(git -C "${HUB_SRC}" rev-parse FETCH_HEAD)"
        if [ "${REMOTE}" != "${STAMPED}" ]; then
            echo "⬆️  hub code updated: ${STAMPED:-(seeded)} → ${REMOTE}"
            git -C "${HUB_SRC}" checkout --quiet main
            git -C "${HUB_SRC}" reset --hard --quiet FETCH_HEAD
            ( cd "${HUB_SRC}" && npm ci --no-audit --no-fund --loglevel=error )
            echo "${REMOTE}" > "${STAMP}"
        fi
    else
        echo "⚠️  hub checkout could not refresh from the fork — using the existing checkout."
        # First-time stamp for a checkout that predates this script.
        if [ ! -f "${STAMP}" ]; then
            git -C "${HUB_SRC}" rev-parse HEAD > "${STAMP}"
        fi
    fi
fi
# Ensure dependencies (covers a seeded checkout without node_modules).
if [ ! -x "${HUB_SRC}/node_modules/.bin/tsx" ]; then
    ( cd "${HUB_SRC}" && npm ci --no-audit --no-fund --loglevel=error )
fi

# --- 3. TLS material (local-only, generated once, AD-4) ------------------------
mkdir -p "${TLS_DIR}"
if [ ! -f "${TLS_DIR}/ca.crt" ]; then
    echo "🔐 generating local CA + hub certificate in ${TLS_DIR}"
    openssl req -x509 -newkey rsa:2048 -keyout "${TLS_DIR}/ca.key" -out "${TLS_DIR}/ca.crt" \
        -days 3650 -nodes -subj "/CN=pi-hermes-memory-CA"
fi
if [ ! -f "${TLS_DIR}/hub.crt" ]; then
    LAN_IP="$(hostname -I 2>/dev/null | awk '{print $1}')"
    openssl req -newkey rsa:2048 -keyout "${TLS_DIR}/hub.key" -out "${TLS_DIR}/hub.csr" \
        -nodes -subj "/CN=cyberkleiber"
    printf "subjectAltName=DNS:cyberkleiber,DNS:localhost,IP:127.0.0.1%s\n" \
        "${LAN_IP:+,IP:${LAN_IP}}" > "${TLS_DIR}/san.cnf"
    openssl x509 -req -in "${TLS_DIR}/hub.csr" -CA "${TLS_DIR}/ca.crt" -CAkey "${TLS_DIR}/ca.key" \
        -CAcreateserial -out "${TLS_DIR}/hub.crt" -days 3650 -extfile "${TLS_DIR}/san.cnf"
    rm -f "${TLS_DIR}/hub.csr" "${TLS_DIR}/san.cnf"
    chmod 0600 "${TLS_DIR}/ca.key" "${TLS_DIR}/hub.key"
fi
mkdir -p "${HUB_DATA}"

# --- 4. launcher + unit ---------------------------------------------------------
# The launcher is also rendered by chezmoi (dot_local/bin); this install keeps
# the run script self-contained and the deployed copy identical to the source.
if [ -f "${LAUNCHER_DEST}" ] && cmp -s "${SRC_DIR}/dot_local/bin/executable_pi-memory-hub-serve.sh" "${LAUNCHER_DEST}"; then
    echo "✅ ${LAUNCHER_DEST} up to date."
else
    install -m 0755 "${SRC_DIR}/dot_local/bin/executable_pi-memory-hub-serve.sh" "${LAUNCHER_DEST}"
    echo "✅ installed ${LAUNCHER_DEST}"
fi
if [ -f "${UNIT_DEST}" ] && cmp -s "${UNIT_SRC}" "${UNIT_DEST}"; then
    echo "✅ ${UNIT_DEST} already installed and up to date."
else
    [ -f "${UNIT_SRC}" ] || { echo "⚠️  unit source not found: ${UNIT_SRC}"; exit 1; }
    [ -f "${UNIT_DEST}" ] && sudo rm -f "${UNIT_DEST}"
    sudo install -o root -g root -m 0644 "${UNIT_SRC}" "${UNIT_DEST}"
    echo "✅ installed ${UNIT_DEST}"
fi
sudo systemctl daemon-reload
sudo systemctl enable "${UNIT}"

# --- 5. start when not already serving ------------------------------------------
# Guard on the healthz probe (not is-active): never restart a live hub.
if curl -fsS --cacert "${TLS_DIR}/ca.crt" "https://127.0.0.1:${PORT}/healthz" >/dev/null 2>&1; then
    echo "✅ hub already serving on :${PORT} — left running."
else
    sudo systemctl start "${UNIT}"
    echo "✅ ${UNIT} started."
fi
