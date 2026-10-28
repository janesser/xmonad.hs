#!/bin/bash
# run_once_5_aitools_8herdr_portfwd.sh
#
# Install the herdr-portfwd plugin (miko-misa/herdr-portfwd) on every box
# that runs herdr — the plugin is needed on both sides of `herdr --remote
# cyberkleiber`: on the server (every action/pane/link-handler runs there) and
# on the machine you sit at (the local agent that actually binds the ports).
#
#   ~/.config/herdr-portfwd/hosts.toml   -> hosts to tunnel (shared)
#   ~/.config/herdr-portfwd/config.toml   -> plugin settings (shared)
#   herdr plugin install ...              -> the plugin itself on the remote
#   ~/.local/bin/herdr-portfwd            -> the CLI (installed by the plugin)
#
# The plugin is needed on BOTH sides of `herdr --remote cyberkleiber`: on
# cyberkleiber (the server, every action/pane/link-handler runs there) and on
# the machine you sit at (the local agent that actually binds the ports). So it
# is installed on every box that runs herdr.
#
# Gated to herdr-only boxes: on any host without herdr this logs and exits 0.
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.
#
# NOTE: hosts.toml (the LAN topology) is still cleartext here — it is an
# age-encrypted blob in a follow-up change (the config dir is rendered on every
# box, so the topology would leak to every host).

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
PLUGIN="miko-misa/herdr-portfwd"
PORTFWD_CONFIG_DIR="${HOME}/.config/herdr-portfwd"
PORTFWD_CLI="${HOME}/.local/bin/herdr-portfwd"

# --- 1. herdr-only gate -------------------------------------------------------
if ! command -v herdr >/dev/null; then
    echo "⏭️  herdr is not installed on $(hostname) — nothing to install."
    exit 0
fi

echo "$(basename "$0"): installing herdr-portfwd on $(hostname) ..."

# --- 2. plugin install ---------------------------------------------------------
# `herdr plugin install` needs git + bash + curl (+ sha256sum/shasum) on the
# box, plus the plugin manifest's build step (bash scripts/install.sh).
if ! command -v git >/dev/null || ! command -v bash >/dev/null || ! command -v curl >/dev/null; then
    echo "⚠️  git/bash/curl are required to install the plugin — missing on this box."
    exit 1
fi

if herdr plugin list 2>/dev/null | grep -q "portfwd"; then
    echo "✅ herdr-portfwd plugin already installed."
else
    echo "📦 installing ${PLUGIN} ..."
    if ! herdr plugin install "${PLUGIN}" --yes; then
        echo "⚠️  herdr plugin install failed on $(hostname)."
        exit 1
    fi
fi

# The plugin registers its actions (and keybindings) through the server config.
herdr server reload-config >/dev/null 2>&1 || {
    echo "⚠️  herdr server reload-config failed on $(hostname)."
    exit 1
}

# --- 3. config files (rendered by chezmoi on every box) ------------------------
# hosts.toml + config.toml are in dot_config/herdr-portfwd/ — chezmoi renders
# them into ~/.config/herdr-portfwd/ on every box. The plugin reads whichever
# of the two files exists first on the machine the command runs on.
for f in hosts.toml config.toml; do
    if [ -f "${SRC_DIR}/dot_config/herdr-portfwd/${f}" ]; then
        DEST="${PORTFWD_CONFIG_DIR}/${f}"
        if [ -f "${DEST}" ] && cmp -s "${SRC_DIR}/dot_config/herdr-portfwd/${f}" "${DEST}"; then
            echo "✅ ${DEST}/${f} up to date."
        else
            mkdir -p "${PORTFWD_CONFIG_DIR}"
            install -m 0644 "${SRC_DIR}/dot_config/herdr-portfwd/${f}" "${DEST}"
            echo "✅ installed ${DEST}/${f}"
        fi
    else
        echo "⚠️  ${f} source not found in dot_config/herdr-portfwd/."
    fi
done

# --- 4. verify ------------------------------------------------------------------
# The install step tries to place the CLI at ~/.local/bin/herdr-portfwd; it is
# best-effort, so verify it actually runs.
if [ -x "${PORTFWD_CLI}" ]; then
    echo "✅ ${PORTFWD_CLI} ready: $(herdr-portfwd --version 2>/dev/null || echo unknown)."
else
    echo "⚠️  herdr-portfwd CLI is not on PATH on $(hostname)."
    echo "   Fix: mkdir -p ~/.local/bin and re-run herdr plugin install ${PLUGIN}."
fi

echo "✅ herdr-portfwd installed on $(hostname) (config from dot_config/herdr-portfwd/)."
