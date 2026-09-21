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

# --- pre-warning to every logged-in session (incl. SSH) -----------------------
# Broadcast via `wall` so the owner sees an on-screen countdown in their SSH
# session / TTY before the cancelable shutdown fires. Guarded: `wall` exits non-
# zero when nobody is logged in (no loginctl entry) or a tty is busy, so it must
# never abort the launcher under `set -e`.
warn_users() {
    local body="$1"
    if command -v wall >/dev/null 2>&1 && who >/dev/null 2>&1; then
        log "broadcasting pre-warning to $(who | wc -l) session(s)"
        wall "${body}" || true
    fi
}

if [ "${DELAY}" -gt 0 ] 2>/dev/null; then
    # Ceil to whole minutes (systemd's `+N` schedule is minute-granular).
    # 300s -> +5.
    MIN=$(( (DELAY + 59) / 60 ))
    log "scheduled poweroff in ${DELAY}s (~${MIN} min) — cancel with 'sudo shutdown -c'"
    # On-screen pre-warning to all logged-in sessions (SSH included). `wall`
    # prepends its own "Broadcast message from <host>" header, so this is just
    # the body.
    warn_users "⏻ auto-poweroff scheduled in ${MIN} min (cancel window open).
              Save your work & disconnect SSH: 'sudo shutdown -c' to cancel."
    # As root (system service) this schedules a cancelable shutdown without a
    # prompt and leaves it pending after this script exits.
    shutdown -P "+${MIN}"
    exit 0
fi

log "powering off now"
systemctl --system poweroff
