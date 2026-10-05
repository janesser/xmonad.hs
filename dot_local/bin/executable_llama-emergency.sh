#!/usr/bin/env bash
# llama-emergency.sh — STOPGAP: free the V100 and serve ONE bare llama model.
#
# Use when you need a working backend fast and want to bypass the managed
# router/Olla path. It stops the managed llama unit(s) that can hold the V100,
# reaps any existing server on the emergency port, then launches:
#
#     llama serve --host 127.0.0.1 --port 9931 -hf <MODEL> --parallel 1 --device CUDA0
#
# (the same "emergency" command, now explicit + --parallel 1 + --device CUDA0).
# --device CUDA0 is REQUIRED: with --parallel 1 the server otherwise ends up
# CPU-bound instead of taking the V100. Port 9931 anticipates the upstream
# llama.cpp default-port switch.
#
# Usage:
#   llama-emergency.sh              # serve the default stopgap model (background)
#   llama-emergency.sh -f           # foreground (exec) — logs to this terminal
#   llama-emergency.sh -m REPO      # serve a different model, e.g. -m Qwen/Qwen3-4B-GGUF:Q4_K_M
#   llama-emergency.sh -p PORT      # use a different emergency port (default 9931)
#   llama-emergency.sh stop         # stop the emergency server (managed units left alone)
#   llama-emergency.sh status       # show what (if anything) is on the emergency port
#
# Notes:
#   * Does NOT mount anything. The HF cache (~/.cache/huggingface/hub) is an
#     fstab bind mount that is up at boot. `mount` is not in the scoped sudoers
#     drop-in, so this script never sudo-mounts (it would block on a password);
#     if the mount is down it warns and llama downloads the model on demand.
#   * Binds the current LAN address, auto-resolved via `ip route get` (so a new
#     DHCP lease keeps the server reachable). Override with LLAMA_HOST.
#   * Stops units via the scoped `systemctl --system` drop-in — non-interactive.
#   * Relies on the caller's shell for HF_TOKEN (as the current stopgap does);
#     nothing is hard-coded here.
set -uo pipefail

#MODEL="unsloth/Qwen3.8-27B-GGUF:Q4_K_M"
MODEL="ornith-ai/Ornith-1.5-35B-A3B-GGUF:Q4_K_M"
HOST="::"
PORT="9931"
SUDO="/usr/bin/systemctl --system"
LOG="$HOME/.local/log/llama-emergency.log"
# Best-effort list of managed units that can hold the V100. Only units that
# actually exist on this box are touched (portable across boxes).
UNITS=(llama-cuda llama-sycl vllm-omni)

usage() {
  awk '/^# Usage:/{f=1;next} /^# Notes:/{f=0} f && /^#   /{sub(/^#   /," ");print}' "$0"
  exit 1
}

MODE="serve"
FG=0
while [ $# -gt 0 ]; do
  case "$1" in
    stop)              MODE="stop"; shift ;;
    status)            MODE="status"; shift ;;
    -f|--foreground)   FG=1; shift ;;
    -m|--model)        MODEL="${2:?model required for $1}"; shift 2 ;;
    -p|--port)         PORT="${2:?port required for $1}"; shift 2 ;;
    -h|--help)         usage ;;
    *) echo "llama-emergency: unknown arg: $1" >&2; usage ;;
  esac
done

# Kill any llama / llama-server process whose cmdline is bound to $1.
reap_port() {
  local port="$1" pid cmd
  for pid in $(pgrep -x llama 2>/dev/null; pgrep -x llama-server 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline")
    case "$cmd" in
      *"$port"*)
        echo "llama-emergency: reaping llama pid $pid on :$port"
        kill "$pid" 2>/dev/null
        for _ in $(seq 1 15); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
        kill -0 "$pid" 2>/dev/null && { echo "  SIGKILL unresponsive pid $pid" >&2; kill -9 "$pid" 2>/dev/null; }
        ;;
    esac
  done
}

port_busy() { ss -ltn 2>/dev/null | awk '{print $4}' | grep -qE "[:.]${PORT}\$"; }

case "$MODE" in
  stop)
    echo "llama-emergency: stopping emergency server on :$PORT"
    reap_port "$PORT"
    exit 0
    ;;
  status)
    if port_busy; then
      echo "llama-emergency: something is listening on :$PORT"
      ss -ltnp 2>/dev/null | grep ":${PORT}\b" || true
    else
      echo "llama-emergency: nothing on :$PORT"
    fi
    exit 0
    ;;
esac

# ---- serve mode ----

# 1. Stop the managed unit(s) that can hold the V100 (frees the GPU for the
#    emergency instance). Non-interactive via the scoped systemctl drop-in.
for u in "${UNITS[@]}"; do
  if "$SUDO" list-unit-files 2>/dev/null | awk '{print $1}' | grep -qx "${u}.service"; then
    if "$SUDO" is-active "$u" 2>/dev/null | grep -qx active; then
      echo "llama-emergency: stopping unit ${u}.service (frees the V100)"
      "$SUDO" stop "$u" || true
    else
      echo "llama-emergency: unit ${u}.service present but not active (skip)"
    fi
  fi
done

# 2. Warn (don't fix) if the HF cache bind mount is down.
if ! mountpoint "$HOME/.cache/huggingface/hub" 2>/dev/null; then
  echo "llama-emergency: WARNING: ~/.cache/huggingface/hub is not a mountpoint." >&2
  echo "  Normally an fstab bind mount. Proceeding; llama will fetch $MODEL on demand." >&2
fi

# 3. Reap anything already bound to the emergency port so the new launch wins.
reap_port "$PORT"
sleep 2

mkdir -p "$(dirname "$LOG")"

if [ "$FG" = 1 ]; then
  echo "llama-emergency: serving $MODEL on http://$HOST:$PORT (foreground; Ctrl-C to stop)"
  exec llama serve --host "$HOST" --port "$PORT" -hf "$MODEL" --parallel 1 --device CUDA0
fi

echo "llama-emergency: serving $MODEL on http://$HOST:$PORT (log: $LOG)"
nohup llama serve --host "$HOST" --port "$PORT" -hf "$MODEL" --parallel 1 --device CUDA0 \
  >>"$LOG" 2>&1 &
PID=$!
disown
echo "llama-emergency: pid $PID — stop with: $0 stop"
HOST="$(hostname -s)"
echo "llama-emergency: waiting on http://$HOST:$PORT/v1/models ..."
for _ in $(seq 1 90); do
  if curl -fsS "http://$HOST:$PORT/v1/models" >/dev/null 2>&1; then
    echo "llama-emergency: READY on :$PORT"
    exit 0
  fi
  sleep 2
done
echo "llama-emergency: :$PORT not ready yet (still loading? check $LOG)" >&2
exit 1
