#!/bin/bash
# standby.sh — mask/unmask sleep & suspend & hibernate & hybrid-sleep targets to
# control whether the machine is allowed to enter standby.
#
# Toggle-with-state + xmobar "barmode", modelled on on-screenlock-toggle.fish:
#   * the bar reads a cheap LOCAL marker (no sudo on every refresh)
#   * only an actual toggle reaches for `sudo systemctl`
#   * `status` reconciles the marker with the real systemd state
#
# Usage:
#   standby.sh toggle   flip between MASKED  (standby disabled) and UNMASKED
#   standby.sh -b       barmode: compact one-line indicator (for xmobar Run Com)
#   standby.sh status   verify the real state against systemd, sync marker, print
#   standby.sh help     this text
#
# The mask/unmask steps need root; the passwordless scope is the visudo drop-in
# /etc/sudoers.d/chezmoi-pi (see the README "pi-agent sudo boundary"). The
# SYSTEMCTL_UNIT alias must grant `mask *` and `unmask *` for the toggle to work.
set -euo pipefail

TARGETS="sleep.target suspend.target hibernate.target hybrid-sleep.target"
STATE_FILE="${XDG_DATA_HOME:-$HOME/.local/share}/standby.state"
MASKED="MASKED"

# current marker (defaults to UNMASKED when nothing has been written yet)
state() { [ -f "$STATE_FILE" ] && cat "$STATE_FILE" 2>/dev/null || echo UNMASKED; }
# persist a marker, creating the state dir if needed
mark() { mkdir -p "$(dirname "$STATE_FILE")"; printf '%s\n' "$1" > "$STATE_FILE"; }

# reconcile the local marker with what systemd really thinks, print it
verify() {
    # list-unit-files prints "<name>  <state>"; a masked target shows "masked".
    # As read-only this is already passwordless (see the sudoers alias).
    if sudo systemctl list-unit-files $TARGETS 2>/dev/null | grep -q masked; then
        mark "$MASKED"
        echo "$MASKED"
    else
        mark UNMASKED
        echo UNMASKED
    fi
}

do_toggle() {
    if [ "$(state)" = "$MASKED" ]; then
        sudo systemctl unmask $TARGETS
        mark UNMASKED
        echo "standby: UNMASKED (sleep/suspend enabled)"
    else
        sudo systemctl mask $TARGETS
        mark "$MASKED"
        echo "standby: MASKED (sleep/suspend disabled)"
    fi
}

case "${1:-status}" in
    help|-h|--help)
        echo "$0 status|toggle|-b"
        ;;
    toggle)
        do_toggle
        ;;
    -b)
        # barmode: compact indicator only, no sudo, fast for the update loop
        [ "$(state)" = "$MASKED" ] && echo "standby: MASKED" || echo "standby: UNMASKED"
        ;;
    status|is-masked)
        echo "standby: $(verify)"
        ;;
    *)
        echo "unknown argument: $1" >&2
        echo "$0 status|toggle|-b" >&2
        exit 2
        ;;
esac
