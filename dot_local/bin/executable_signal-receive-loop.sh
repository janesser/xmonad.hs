#!/bin/bash
# signal-receive-loop.sh — signal-cli daemon on raspberrypi, bound to all interfaces
# so every pi-agent instance in the fleet can reach the daemon over LAN.

if [ -z "$PI_SIGNAL_ACCOUNT" ]; then
  ACCOUNTS_FILE="$HOME/.local/share/signal-cli/data/accounts.json"
  if [ -f "$ACCOUNTS_FILE" ]; then
    PI_SIGNAL_ACCOUNT=$(grep -o '"number"[[:space:]]*:[[:space:]]*"[^"]*"' "$ACCOUNTS_FILE" | head -1 | sed 's/.*"\([+0-9]*\)"$/\1/' || true)
  fi
fi

if [ "${PI_SIGNAL_QUIET_DAEMON:-true}" = "true" ] || \
   [ "${PI_SIGNAL_QUIET_DAEMON:-true}" = "1" ] || \
   [ "${PI_SIGNAL_QUIET_DAEMON:-true}" = "yes" ]; then
  DAEMON_REDIRECT=">/dev/null 2>&1"
else
  DAEMON_REDIRECT=""
fi

DAEMON_PID=""

cleanup() {
  if [ -n "$DAEMON_PID" ]; then
    kill "$DAEMON_PID" 2>/dev/null
    wait "$DAEMON_PID" 2>/dev/null
  fi
  exit 0
}
trap cleanup EXIT INT TERM

pkill -f "signal-cli daemon --http" 2>/dev/null || true
sleep 1
pkill -9 -f "signal-cli daemon --http" 2>/dev/null || true
sleep 1

eval "signal-cli -a \"\$PI_SIGNAL_ACCOUNT\" daemon --http 0.0.0.0:8081 $DAEMON_REDIRECT &"
DAEMON_PID=$!

echo "Waiting for signal-cli daemon (PID $DAEMON_PID)..."
DAEMON_READY=0
for _ in $(seq 1 60); do
  if curl -s http://127.0.0.1:8081/api/v1/check >/dev/null 2>&1; then
    echo "signal-cli daemon is ready"
    DAEMON_READY=1
    break
  fi
  if ! kill -0 "$DAEMON_PID" 2>/dev/null; then
    echo "signal-cli daemon exited unexpectedly" >&2
    exit 1
  fi
  sleep 1
done

if [ "$DAEMON_READY" -ne 1 ]; then
  echo "ERROR: signal-cli daemon did not start within 60s" >&2
  exit 1
fi

echo "signal-cli daemon is running on all interfaces. pi will connect to the SSE endpoint over LAN."

while true; do
  if ! kill -0 "$DAEMON_PID" 2>/dev/null; then
    echo "signal-cli daemon died, restarting..."
    eval "signal-cli -a \"\$PI_SIGNAL_ACCOUNT\" daemon --http 0.0.0.0:8081 $DAEMON_REDIRECT &"
    DAEMON_PID=$!
    sleep 5
  fi

  if ! curl -s --max-time 5 http://127.0.0.1:8081/api/v1/check >/dev/null 2>&1; then
    echo "signal-cli daemon health check failed, restarting..."
    kill "$DAEMON_PID" 2>/dev/null
    wait "$DAEMON_PID" 2>/dev/null
    eval "signal-cli -a \"\$PI_SIGNAL_ACCOUNT\" daemon --http 0.0.0.0:8081 $DAEMON_REDIRECT &"
    DAEMON_PID=$!
    sleep 5
  fi

  sleep 30
done
