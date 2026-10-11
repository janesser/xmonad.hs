#!/bin/bash
# llama-handover.sh — mutual-support executor (cyberkleiber).
#
# Runs as a tracked pi-command (inherits PI_SESSION_ID). PRECONDITION:
# llama-sycl (:8082) is ALREADY up. The executor's job is to tear down the CUDA
# backend (:8081, freeing the V100) and confirm the SYCL backend is serving. It
# does NOT clear the marker — the marker holds the ready-to-run `pi_command`
# (written by llama-handover-request.sh, or filled in here if sycl was down at
# request time) so a resumed caller can read it and run it. The marker is
# cleared on return (EVENT=restored) or cancel. On any abort the marker is KEPT
# and an EVENT=aborted line is logged so the outcome is inspectable.
#
# Model is auto-picked: first id from `curl :8082/v1/models` (jan prefers the
# currently available model over any hardcoded one). No manual model decision.
set -uo pipefail

SHARED="$HOME/.local/share/llama-cpp"
DIR="$SHARED/handover"
REQ="$DIR/requested"
LOG="$DIR/handover.log"
SESSION="${PI_SESSION_ID:-unknown}"

log() { echo "$(date --iso-8601=seconds) | SESSION=${SESSION} | EVENT=$1 | $2" >> "$LOG"; }
abort() { echo "llama-handover: $1"; log aborted "$1"; exit 1; }   # marker kept

# Read a JSON string field (python3 if present, else a grep fallback).
json_field() {
  if command -v python3 >/dev/null 2>&1; then
    python3 -c "import json,sys; print(json.load(open(sys.argv[1])).get(sys.argv[2],''))" "$REQ" "$1"
  else
    grep -o "\"$1\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" "$REQ" | head -n1 | sed 's/.*: *"//; s/"$//'
  fi
}

[ -f "$REQ" ] || { echo "llama-handover: no pending marker — nothing to do."; exit 0; }

echo "llama-handover: marker present (requested_by_session=$(json_field requested_by_session))."

# 1) Tear down the CUDA backend (:8081) — the executor owns this shutdown, so
#    the caller no longer runs `systemctl stop llama-cuda` itself.
echo "llama-handover: stopping llama-cuda (:8081) to free the V100."
sudo systemctl stop llama-cuda
if curl -fsS --max-time 3 http://127.0.0.1:8081/v1/models >/dev/null 2>&1; then
  abort ":8081 still up after stop — marker kept, retry."
fi
echo "llama-handover: :8081 down."

# 2) llama-sycl (:8082) is assumed already up — just confirm it answers.
if ! curl -fsS --max-time 3 http://127.0.0.1:8082/v1/models >/dev/null 2>&1; then
  abort ":8082 not reachable — llama-sycl not up, marker kept."
fi
echo "llama-handover: :8082 (llama-sycl) is serving."

# 3) Resolve the ready pi command for the resumed caller. The model is normally
#    pre-picked by llama-handover-request.sh (embedded as pi_command/handover_model);
#    fill it in now only if sycl was down at request time.
HCID="crossbar-llamacpp-cyberkleiber-fritz-box-8082"
PI_CMD="$(json_field pi_command)"
if [ -z "$PI_CMD" ]; then
  MODEL="$(curl -fsS --max-time 3 http://127.0.0.1:8082/v1/models 2>/dev/null \
    | python3 -c 'import json,sys;print(json.load(sys.stdin)["data"][0]["id"])' 2>/dev/null)"
  PI_CMD="pi --provider $HCID"
  [ -n "$MODEL" ] && PI_CMD="$PI_CMD --model '$MODEL'"
  # persist the picked command so a resumed caller can read it from the marker
  python3 - "$REQ" "$MODEL" "$PI_CMD" <<'PY' || true
import json,sys
req,model,pic=sys.argv[1],sys.argv[2],sys.argv[3]
d=json.load(open(req))
d["handover_model"]=model
d["pi_command"]=pic
json.dump(d,open(req,"w"),indent=2)
PY
fi

# 4) Handover = run pi against llama-sycl with a per-invocation --provider override
#    (requires --model). This does NOT touch the persisted defaultProvider.
echo "llama-handover: caller should run -> eval \"$PI_CMD\"  (per-invocation --provider; defaultProvider untouched)."

# 5) Log completion. Marker is LEFT in place so the resumed caller can read the
#    pi command from it; it is cleared on return/cancel.
log completed ":8081 down; :8082 serving; pi_command='$PI_CMD'"
echo "llama-handover: handover complete — V100 freed, serving via llama-sycl (:8082). Marker left for caller to read."
