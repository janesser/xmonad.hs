#!/bin/bash
# wakeup.cyberkleiber
#
# Wake the target machine (Wake-on-LAN magic packet) three times, then poll
# until it answers and exec a btop session on it, so callers never race a
# machine that is still booting.
#
# The target is identified by MAC for wake (WAKE_MAC) and by hostname for the
# availability poll. The target hostname defaults to `cyberkleiber`; override
# it with WAKE_HOSTNAME (or pass $1) to wake/ping/ssh a different host/IP.
#
# Tunables (env): WAKE_MAC, WAKE_TIMES, WAKE_HOSTNAME, WAKE_POLL_INTERVAL,
#   WAKE_PING_TIMEOUT
set -uo pipefail

[[ -z "$TARGET_MAC" ]] && exit 1
[[ -z "$HOSTNAME" ]] && exit 1

SCRIPT="$(basename "$0")"

WAKE_TIMES="${WAKE_TIMES:-3}"
POLL_INTERVAL="${WAKE_POLL_INTERVAL:-5}"   # seconds between availability polls
PING_TIMEOUT="${WAKE_PING_TIMEOUT:-5}"     # per-ping timeout (seconds)
# Hostname/IP to ping and ssh. Defaults to `cyberkleiber`.


# 1. Wake three times (extra attempts cover a missed/lost first packet).
for i in $(seq 1 "$WAKE_TIMES"); do
    echo "$SCRIPT: sending wake packet ($i/$WAKE_TIMES) to $TARGET_MAC"
    wakeonlan "$TARGET_MAC"
    sleep 1
done

# 2. Poll until the target becomes available, then run btop on it.
echo "$SCRIPT: $HOSTNAME is asleep; polling until reachable (ping)..."
while true; do
    if ping -c1 -W "$PING_TIMEOUT" "$HOSTNAME" >/dev/null 2>&1; then
        echo "$SCRIPT: $HOSTNAME is now reachable."
        exit 0
    fi
    sleep "$POLL_INTERVAL"
done
