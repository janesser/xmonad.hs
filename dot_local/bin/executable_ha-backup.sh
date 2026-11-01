#!/bin/bash
# ha-backup.sh
#
# Backup the Home Assistant config to ~/Nextcloud/ha-backup/ (Nextcloud-synced).
#
# Deployed by chezmoi (.chezmoiscripts/run_onchange_hass_backup.sh.tmpl) to
# /usr/local/bin, run as `jan` by the systemd oneshot service
# `ha-backup.service`, triggered by `ha-backup.timer` (daily 03:00).
#
# Retains the last 7 daily backups. The HA config dir is the container's
# persistent /config volume on the host:
#   ~/.local/share/homeassistant/config/
#
# Sudo is not needed here — the service runs as `jan` and both paths are
# user-owned.

set -euo pipefail

HA_CONFIG_DIR="$HOME/.local/share/homeassistant/config"
BACKUP_DIR="$HOME/Nextcloud/ha-backup"
RETAIN=7

[ -d "${HA_CONFIG_DIR}" ] || { echo "⚠️  HA config dir missing: ${HA_CONFIG_DIR}"; exit 1; }
[ -d "${BACKUP_DIR}" ] || mkdir -p "${BACKUP_DIR}"

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
ARCHIVE="${BACKUP_DIR}/homeassistant-config_${TIMESTAMP}.tar.gz"

tar czf "${ARCHIVE}" -C "$HOME/.local/share/homeassistant" config

# --- rotate (keep the last RETAIN daily backups) -----------------------------
cd "${BACKUP_DIR}"
BACKUPS="$(ls -t homeassistant-config_*.tar.gz 2>/dev/null || true)"
COUNT="$(echo "${BACKUPS}" | wc -l)"
if [ "${COUNT}" -gt "${RETAIN}" ]; then
    echo "${BACKUPS}" | tail -n "+$((RETAIN+1))" | while IFS= read -r f; do
        rm -f "${BACKUP_DIR}/${f}"
        echo "🗑️  removed old backup: ${f}"
    done
fi

echo "✅ HA config backup: ${ARCHIVE} ($(du -h "${ARCHIVE}" | cut -f1))"
