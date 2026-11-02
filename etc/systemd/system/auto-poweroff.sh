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
# 1. `wall` at best — broadcasts to every entry in /run/utmp (SSH + TTY).
# 2. Direct write to /dev/pts/* if `wall` has no targets: this box ships
#    systemd built without -UTMP, so /run/utmp is absent and `wall` always
#    delivers to nobody. Root can still write straight onto the ptys.
warn_users() {
    local body="$1"

    if command -v wall >/dev/null 2>&1; then
        log "broadcasting pre-warning via wall"
        wall "${body}" || true
    fi

    # `wall` is silent when /run/utmp has no entries (this release). Write the
    # warning straight onto every live pty so it always lands.
    local n=0
    for pty in /dev/pts/[0-9]*; do
        [ -w "$pty" ] || continue
        printf '%s\n' "${body}" >"$pty" 2>/dev/null || continue
        n=$((n + 1))
    done
    log "pre-warning written to ${n} pty(s)"
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
