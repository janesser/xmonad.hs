#!/usr/bin/env bash
# card2-swap.sh — on-demand sd.cpp image batches on card 2 (PCIe V100),
# WITHOUT evicting the main 27B text backend on CUDA0 (:8081) or vLLM-Omni.
#
#   image --batch FILE [--deadline 900] [--detach] [--keep-text]
#       Stop the little brother (:8082) — unless --keep-text — run the sd.cpp
#       batch pinned to card 2 (UUID read from the unit file), then start the
#       little brother again. Single-flight via flock; mode tracked in a state
#       file. --detach runs the same sequence as a transient systemd unit
#       (card2-image-<timestamp>) so the job outlives the calling session; the
#       caller gets the unit name back immediately.
#   text
#       Start the little brother (bounded readiness wait). The recovery path.
#   status
#       Show the state file + unit liveness; surfaces a stale 'swapping'.
#
# The batch itself runs through switch-ai-backend.sh image --no-evict (the
# verified batch loop: per-call watchdog deadline, results JSON). Success is
# judged from the results JSON (every call 'ok'), NEVER from the runner's
# exit code, which stays 0 on per-call failure.
#
# sudo (scoped NOPASSWD drop-in) is used for systemctl start/stop and
# systemd-run (--detach job units) only; unit state queries are read-only and
# run without sudo.

set -uo pipefail

PORT=8082
TEXT_UNIT="llama-little-brother"
JOB_GLOB="card2-image-*"
UNIT_FILE="/etc/systemd/system/llama-little-brother.service"
STATE_FILE="${HOME}/.local/share/card2/state"
LOCK_FILE="${HOME}/.local/share/card2/lock"
SWITCH="${HOME}/.local/bin/switch-ai-backend.sh"
DEFAULT_DEADLINE=900

log() { echo "card2-swap: $*"; }
die() {
    echo "card2-swap: ERROR: $*" >&2
    logger -t card2-swap "ERROR: $*" 2>/dev/null || true
    exit 1
}

# The unit file is the single source of truth for WHICH card this system owns
# (Era 2: re-pin the card = edit that one line).
require_card2() {
    [ -f "$UNIT_FILE" ] || die "no card-2 unit installed here ($UNIT_FILE missing — box without the card?)"
    local u
    u="$(sed -n 's/^Environment=CUDA_VISIBLE_DEVICES=//p' "$UNIT_FILE" | head -1)"
    [ -n "$u" ] || die "no CUDA_VISIBLE_DEVICES UUID in $UNIT_FILE"
}

state_get() {
    if [ -f "$STATE_FILE" ]; then awk -F'|' '{print $1}' "$STATE_FILE"; else echo unknown; fi
}

state_set() { # MODE RESULT
    mkdir -p "$(dirname "$STATE_FILE")"
    echo "$1|$(date +%s)|$2" > "$STATE_FILE"
}

wait_ready() {
    local i
    for i in $(seq 1 150); do
        curl -fsS "http://127.0.0.1:${PORT}/v1/models" >/dev/null 2>&1 && return 0
        sleep 2
    done
    return 1
}

mark_degraded() {
    state_set degraded "text backend not running"
    logger -t card2-swap "DEGRADED: text backend on card 2 not running — manual recovery: ${HOME}/.local/bin/card2-swap.sh text (journal: journalctl -u ${TEXT_UNIT})" 2>/dev/null || true
}

# Start the text unit and wait for :8082. Two failure modes need guarding:
#  - after a crash loop, systemd rate-limits rapid start requests ("Start
#    request repeated too quickly", 10s window) and reset-failed is not in
#    the drop-in → retry with a 15s ride-out;
#  - `systemctl start` can BLOCK its client indefinitely while the unit is
#    stuck activating/crash-looping → client-side timeout on every call.
start_text_unit() {
    local try
    for try in 1 2 3; do
        if timeout 120 sudo systemctl start "${TEXT_UNIT}" 2>/dev/null && wait_ready; then
            return 0
        fi
        sleep 15
    done
    return 1
}

# --- image sequence (runs foreground, or re-invoked inside the job unit) ----
PHASE=""            # "swapping" while the stop/batch/start sequence is in flight
STOPPED_TEXT=0      # 1 once we stopped the text unit (we must bring it back)
RESTORED=0          # 1 once the text unit is confirmed running again
RESULT="failed"     # ok|failed, from the batch results JSON
STATE_FINAL=0       # 1 once the final state has been written for this run

on_exit() {
    local rc=$?
    [ "$PHASE" = "swapping" ] || return 0
    PHASE=""
    # Backstop: if we stopped the text unit, it must be back before we exit
    # (covers die/timeout/TERM between the stop and the explicit start).
    if [ "${STOPPED_TEXT}" = "1" ] && [ "${RESTORED}" != "1" ]; then
        start_text_unit && RESTORED=1 || true
    fi
    if [ "${STATE_FINAL}" != "1" ]; then
        if systemctl is-active --quiet "${TEXT_UNIT}" 2>/dev/null || [ "${RESTORED}" = "1" ]; then
            state_set text "${RESULT}"
        else
            mark_degraded
        fi
    fi
    return 0
}
trap on_exit EXIT

cmd_image() {
    local batch="$1" deadline="$2" detach="$3" keep_text="$4"
    [ -f "$batch" ] || die "batch file not found: $batch"
    require_card2
    local uuid
    uuid="$(sed -n 's/^Environment=CUDA_VISIBLE_DEVICES=//p' "$UNIT_FILE" | head -1)"

    if [ "${detach}" = "1" ]; then
        # Single-flight, part 1: refuse while any job unit is active.
        local active_jobs
        active_jobs="$(systemctl list-units --state=active --plain --no-legend "${JOB_GLOB}" 2>/dev/null | awk '{print $1}')"
        [ -z "$active_jobs" ] || die "already swapping (job unit active: $(echo "$active_jobs" | tr '\n' ' ')) — see: card2-swap.sh status"
        local self ts unitname keeparg
        self="$(readlink -f "$0")"
        ts="$(date +%s)"
        unitname="card2-image-${ts}"
        keeparg=""; [ "${keep_text}" = "1" ] && keeparg="--keep-text"
        # The sequence re-invokes ITSELF inside the transient unit: system-scoped
        # (survives any session), the process runs as jan (state/lock ownership,
        # and the inner sudo still works), stdout lands in `journalctl -u unit`.
        # The inner run holds the flock — a losing double-detach fails the lock
        # and exits BEFORE touching any backend.
        if ! sudo systemd-run --unit="${unitname}" \
                --uid="$(id -u)" --gid="$(id -g)" \
                --setenv=HOME=/home/jan --setenv=USER=jan \
                -- /usr/bin/bash "${self}" image --batch "${batch}" --deadline "${deadline}" ${keeparg}; then
            die "detach failed (is the systemd-run grant installed in the sudoers drop-in?)"
        fi
        log "detached: job unit ${unitname} (journal: journalctl -u ${unitname})"
        return 0
    fi

    # Single-flight, part 2: the lock (fd auto-release; no stale-lock recovery).
    mkdir -p "$(dirname "$LOCK_FILE")"
    exec 9>"$LOCK_FILE" || die "cannot open lock file: $LOCK_FILE"
    flock -n 9 || die "already swapping (lock held) — see: card2-swap.sh status"

    PHASE=swapping
    state_set swapping "batch started"
    log "state=swapping (batch ${batch}, deadline ${deadline}s, keep-text=${keep_text})"

    if [ "${keep_text}" = "0" ]; then
        timeout 150 sudo systemctl stop "${TEXT_UNIT}" || die "failed to stop ${TEXT_UNIT}"
        STOPPED_TEXT=1
    fi

    # The batch, pinned to card 2; --no-evict keeps :8081 / vLLM-Omni alone.
    local out batch_rc
    out="$(CUDA_VISIBLE_DEVICES="${uuid}" "${SWITCH}" image --batch "${batch}" --deadline "${deadline}" --no-evict)"
    batch_rc=$?
    echo "$out"

    # Success = every call ok in the results JSON (printed last, first '[' at
    # line start; the runner's own exit code does not reflect per-call failure).
    if echo "$out" | python3 -c '
import json, sys
s = sys.stdin.read()
dec = json.JSONDecoder()
# The results array is the last JSON array in the stream. The runner prints a
# summary line AFTER it, so use raw_decode (tolerates trailing text). sd-cli
# log/progress lines containing "[" ("[I] ...", "[K") never decode as JSON.
idx = len(s)
while True:
    i = s.rfind("[", 0, idx)
    if i == -1:
        break
    try:
        r, _ = dec.raw_decode(s[i:])
    except Exception:
        idx = i
        continue
    if isinstance(r, list):
        sys.exit(0 if r and all(isinstance(x, dict) and x.get("ok") for x in r) else 1)
    idx = i
sys.exit(1)
' 2>/dev/null; then
        RESULT=ok
    else
        RESULT=failed
        [ "${batch_rc}" = "0" ] || log "runner exited ${batch_rc}"
        logger -t card2-swap "image batch FAILED (result: see this run's stdout/journal)" 2>/dev/null || true
    fi

    if [ "${keep_text}" = "1" ]; then
        RESTORED=1    # the text unit was never stopped
    elif start_text_unit; then
        RESTORED=1
    fi

    STATE_FINAL=1
    if [ "${RESTORED}" = "1" ]; then
        state_set text "${RESULT}"
        log "state=text (result=${RESULT})"
    else
        mark_degraded
    fi
    [ "${RESULT}" = "ok" ]
}

cmd_text() {
    require_card2
    if start_text_unit; then
        state_set text ok
        log "text mode: ${TEXT_UNIT} active, :${PORT} ready."
    else
        mark_degraded
        die "text backend did not come up — journal: journalctl -u ${TEXT_UNIT}"
    fi
}

cmd_status() {
    local mode text_active jobs port_ok
    mode="$(state_get)"
    text_active="$(systemctl is-active "${TEXT_UNIT}" 2>/dev/null || true)"
    jobs="$(systemctl list-units --state=active --plain --no-legend "${JOB_GLOB}" 2>/dev/null | awk '{print $1}' | tr '\n' ' ')"
    [ -n "$jobs" ] || jobs="none"
    # The state file records what the script last did; the port probe records
    # what is TRULY answering right now (a state written against a process
    # that later died would otherwise read as 'text').
    port_ok="down"
    curl -fsS --max-time 3 "http://127.0.0.1:${PORT}/v1/models" >/dev/null 2>&1 && port_ok="serving"
    echo "card2-swap state: $( [ -f "$STATE_FILE" ] && cat "$STATE_FILE" || echo '(no state file)' )"
    echo "text unit ${TEXT_UNIT}: ${text_active:-missing} (port :${PORT}: ${port_ok})"
    echo "job units: ${jobs}"
    if [ "$mode" = "swapping" ] && [ "$jobs" = "none" ]; then
        echo "card2-swap: WARNING: state says 'swapping' but no job unit is running — stale state (killed job?). If the text unit is down, recover with: card2-swap.sh text"
    fi
    return 0
}

usage() {
    cat >&2 <<EOF
usage:
  $0 image --batch FILE [--deadline SECONDS] [--detach] [--keep-text]
      Run an sd.cpp image batch on card 2 (UUID-pinned), without touching the
      27B on :8081 or vLLM-Omni. Stops the little brother (:8082) unless
      --keep-text, and restores it however the batch exits.
        --deadline S   batch deadline in seconds (default ${DEFAULT_DEADLINE})
        --detach       run the sequence as a transient unit (card2-image-<ts>);
                       returns the unit name immediately (needs the
                       systemd-run grant in the sudoers drop-in)
        --keep-text    do not stop the little brother; co-reside with the batch
  $0 text         Start the little brother (recovery) + readiness wait
  $0 status       Show state file + unit liveness
EOF
}

case "${1:-}" in
    image)
        shift
        BATCH=""; DEADLINE="$DEFAULT_DEADLINE"; DETACH=0; KEEP_TEXT=0
        while [ $# -gt 0 ]; do
            case "$1" in
                --batch) BATCH="${2:-}"; shift 2 ;;
                --deadline) DEADLINE="${2:-}"; shift 2 ;;
                --detach) DETACH=1; shift ;;
                --keep-text) KEEP_TEXT=1; shift ;;
                *) usage; exit 2 ;;
            esac
        done
        [ -n "$BATCH" ] || { usage; exit 2; }
        cmd_image "$BATCH" "$DEADLINE" "$DETACH" "$KEEP_TEXT"
        ;;
    text)
        cmd_text
        ;;
    status)
        cmd_status
        ;;
    *)
        usage; exit 2
        ;;
esac
