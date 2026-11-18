#!/bin/bash
# signal-pi-status.sh — status of the signal-pi chain, from Signal to llama.cpp
#
#   1. signal-cli daemon (raspberrypi:8081)           — receives messages
#   2. pi agent + SSE stream                          — the bot itself
#   3. cyberkleiber fleet members (:8081/:8082)       — the llama.cpp backends
#
# Check 2 is stronger than `systemctl is-active signal-pi`: that reports 'active'
# even when systemd has lost track of the process (MainPID=0). The SSE stream
# state lives in-memory in the agent, so this reads the last `[signal] SSE ...`
# line from the service log instead.
#
# Check 3 uses the same endpoint as the readiness gate (`/v1/health`, which
# returns 200 only when the model is serving). A plain port check would call a
# server 'up' while its backend was actually down — and that's what delayed every
# message by ~150s of failed readiness retries before.

set -uo pipefail

AGENT_LOG="${PI_AGENT_LOG:-$HOME/.pi/agent/signal-pi.log}"
DAEMON="http://127.0.0.1:8081"          # signal-cli daemon (signal-receive.service)
ACCOUNT="+4917620128275"               # PI_SIGNAL_ACCOUNT

# ── helpers ────────────────────────────────────────────────────────────────
row() { printf "| %-34s | %-28s |\n" "$1" "$2"; }
sep() { printf "| %-34s + %-28s |\n" "------------------------------" "-----------------------------"; }

# ── 1. signal-cli daemon (the JSON-RPC/SSE server) -------------------------
if curl -s --max-time 3 "${DAEMON}/api/v1/check" >/dev/null 2>&1; then
    DAEMON_STATE="up"
else
    DAEMON_STATE="down"
fi

# ── 2. pi agent + SSE stream (the bot) -------------------------------------
AGENT_PID=$(pgrep -x pi 2>/dev/null | head -1)
if [ -n "$AGENT_PID" ] && kill -0 "$AGENT_PID" 2>/dev/null; then
    SSE_LINE=$(grep -a "\[signal\] SSE" "$AGENT_LOG" 2>/dev/null | tail -1)
    if [[ "$SSE_LINE" == *"connected"* ]]; then
        detail="up (SSE connected)"
    elif [[ "$SSE_LINE" == *"reconnecting"* ]]; then
        detail="reconnecting (${SSE_LINE##*signal] })"
    elif [ -n "$SSE_LINE" ]; then
        detail="up, stream state unknown (last: ${SSE_LINE##*signal] })"
    else
        detail="up (agent running, no log data)"
    fi
else
    detail="down (process not running)"
fi

# ── 3. cyberkleiber fleet members (the llama.cpp backends) -----------------
for port in 8081 8082; do
    CODE=$(timeout 5 curl -s --max-time 5 -o /dev/null -w "%{http_code}" \
        "http://cyberkleiber.fritz.box:${port}/v1/health" 2>/dev/null)
    if [ "$CODE" = "200" ]; then
        CYBER_STATE="up (model serving)"
    elif [ -n "$CODE" ]; then
        CYBER_STATE="reachable, not ready ($CODE)"
    else
        CYBER_STATE="down (no response within 5s)"
    fi
    eval "CYBER_${port}='$CYBER_STATE'"
done

# ── output: table ----------------------------------------------------------
echo ""
row "component" "status"
sep
row "signal-cli daemon :8081" "$DAEMON_STATE"
row "pi agent (signal-pi) + SSE" "$detail"
row "cyberkleiber :8081" "${CYBER_8081}"
row "cyberkleiber :8082" "${CYBER_8082}"
echo ""

# Detail: what the llama backend is actually occupying, if anything.
pgrep -a llama-server 2>/dev/null | cut -c1-100
ss -ltnp 2>/dev/null | grep -E "49374|8081" | cut -c1-100 || true
