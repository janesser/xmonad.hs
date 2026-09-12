#!/bin/bash
# auto-poweroff — launcher for the chezmoi-managed auto-poweroff systemd service.
# Installed to /usr/local/bin by run_onchange_9_1_auto_poweroff_timer.sh.
# Runs as root (system timer).
#
#   delay > 0 : schedule a CANCELLABLE shutdown (`shutdown -P +<min>`) so the
#               owner can cancel with `sudo shutdown -c` during the window.
#   delay == 0: power off immediately (no cancel window).
set -euo pipefail

DELAY="${1:-0}"

log() { logger -t auto-poweroff "$@"; }

if [ "${DELAY}" -gt 0 ] 2>/dev/null; then
    # Ceil to whole minutes (systemd's `+N` schedule is minute-granular).
    # 300s -> +5.
    MIN=$(( (DELAY + 59) / 60 ))
    log "scheduled poweroff in ${DELAY}s (~${MIN} min) — cancel with 'sudo shutdown -c'"
    # As root (system service) this schedules a cancelable shutdown without a
    # prompt and leaves it pending after this script exits.
    shutdown -P "+${MIN}"
    exit 0
fi

log "powering off now"
systemctl --system poweroff
