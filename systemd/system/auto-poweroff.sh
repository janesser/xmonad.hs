#!/bin/bash
# auto-poweroff — launcher for the chezmoi-managed auto-poweroff systemd service.
# Installed to /usr/local/bin by run_onchange_9_1_auto_poweroff_timer.sh.
# Runs as root (system timer). Logs a warning, waits an optional grace delay,
# then powers the machine off.
#
# Usage: auto-poweroff.sh [DELAY_SECONDS]
set -euo pipefail

DELAY="${1:-0}"

log() { logger -t auto-poweroff "$@"; }

if [ "${DELAY}" -gt 0 ] 2>/dev/null; then
    log "scheduled poweroff in ${DELAY}s"
    # Interruptible sleep: a later cancel-shutdown can still stop us.
    sleep "${DELAY}" || true
fi

log "powering off now"
systemctl --system poweroff
