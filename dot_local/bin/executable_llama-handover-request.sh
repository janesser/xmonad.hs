#!/bin/bash
# llama-handover-request.sh — write the mutual-support handover marker when
# llama-cuda (:8081) is unloaded/stopped on cyberkleiber, so the protocol can
# hand serving over to llama-sycl (:8082).
#
# This is the MANDATORY trigger: before you stop/unload llama-cuda on
# cyberkleiber, run this so the handover is recorded and attributable. The pi
# session id (PI_SESSION_ID) is stamped on the marker and the event log.
#
# The handover model is auto-picked: this script curls :8082/v1/models and passes
# the first id to the handover `pi --provider … --model …` (jan prefers the
# currently available model over any hardcoded one). The ready-to-run pi command
# is embedded in the marker as `pi_command` so a resumed caller can just read it
# and run it. If :8082 is not reachable yet, the model is left blank and the
# executor fills it in once llama-sycl is up.
#
# Usage:
#   llama-handover-request.sh [reason] [trigger]
#     reason   short reason (default: stop)
#     trigger  what caused the stop  (default: `systemctl stop llama-cuda`)
set -euo pipefail

SHARED="$HOME/.local/share/llama-cpp"
DIR="$SHARED/handover"
mkdir -p "$DIR"

SESSION="${PI_SESSION_ID:-unknown}"
REASON="${1:-stop}"
TRIGGER="${2:-systemctl stop llama-cuda}"
NOW="$(date --iso-8601=seconds)"

# Pick the handover model from what llama-sycl (:8082) currently exposes first.
MODEL="$(curl -fsS --max-time 3 http://127.0.0.1:8082/v1/models 2>/dev/null \
  | python3 -c 'import json,sys;print(json.load(sys.stdin)["data"][0]["id"])' 2>/dev/null)"
HCID="crossbar-llamacpp-cyberkleiber-fritz-box-8082"
PI_CMD="pi --provider $HCID"
[ -n "$MODEL" ] && PI_CMD="$PI_CMD --model '$MODEL'"

REQ="$DIR/requested"
TMP="$(mktemp "${DIR}/.requested.XXXXXX")"
cat > "$TMP" <<EOF
{
  "host": "cyberkleiber",
  "from": {"unit": "llama-cuda", "port": 8081, "crossbar": "crossbar-llamacpp-cyberkleiber-fritz-box-8081"},
  "to":   {"unit": "llama-sycl", "port": 8082, "crossbar": "crossbar-llamacpp-cyberkleiber-fritz-box-8082"},
  "requested_at": "$NOW",
  "requested_by_session": "$SESSION",
  "triggered_by": "$TRIGGER",
  "reason": "$REASON",
  "handover_model": "$MODEL",
  "pi_command": "$PI_CMD"
}
EOF
mv -f "$TMP" "$REQ"

# Note the request in the append-only event log too.
echo "${NOW} | SESSION=${SESSION} | EVENT=requested | from=llama-cuda:8081 to=llama-sycl:8082 model='${MODEL:-pending}' pi='${PI_CMD}' (reason=${REASON})" >> "$DIR/handover.log"

echo "llama-handover-request: wrote $REQ (session=${SESSION}, reason=${REASON}, pi='${PI_CMD}')"

# Step 2 — trigger the executor as a result of this call. The marker write is
# what runs it. The executor owns the CUDA shutdown and inherits PI_SESSION_ID
# from this script's environment (no separate pi-command, no extra --provider).
HANDOVER="$HOME/.local/bin/llama-handover.sh"
if [ -x "$HANDOVER" ]; then
  echo "llama-handover-request: triggering executor ($HANDOVER)."
  if ! "$HANDOVER"; then
    echo "llama-handover-request: executor failed — marker kept (retryable)." >&2
    exit 1
  fi
else
  echo "llama-handover-request: executor not found at $HANDOVER — marker written but executor NOT triggered." >&2
fi
