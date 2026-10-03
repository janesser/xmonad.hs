#!/usr/bin/env bash
# switch-ai-backend.sh {omni|instruct} [MODEL]
#
# Swap which backend holds the single V100 (32 GB). Only ONE can hold it:
#   * instruct -> llama.cpp CUDA backend  (:8081, ornith)  [llama-cuda.service, boot default]
#   * omni     -> vLLM-Omni               (:8091, Qwen2.5-Omni-7B by default)
#
# Olla (:40114) routes each request by model id to whichever backend is live
# and picks the change up on its discovery refresh. This script does NOT restart
# Olla — its ExecStartPre requires :8081 reachable, so restarting while vLLM-Omni
# holds the V100 (no :8081) would fail; Olla's periodic refresh is enough.
#
# sudo (NOPASSWD drop-in, /usr/bin/systemctl --system) is used ONLY to manage the
# llama-cuda system unit. vLLM-Omni is a plain background process started by
# restart-vllm-omni.sh. Reaping the llama backend does NOT umount the shared HF
# cache bind mount — it must stay mounted for vLLM-Omni.
set -uo pipefail

RESTART_OMNI="$HOME/.local/bin/restart-vllm-omni.sh"
SUDO="/usr/bin/systemctl --system"

reap() {
  # port-bound reap of a named binary family (llama / api_server) on $1.
  local family="$1" port="$2" pid cmd
  for pid in $(pgrep -x "$family" 2>/dev/null); do
    [ -r "/proc/$pid/cmdline" ] || continue
    cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null)
    case "$cmd" in *"$port"*)
      echo "switch: reaping $family pid $pid on :$port"
      kill "$pid" 2>/dev/null
      for _ in $(seq 1 15); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
      kill -0 "$pid" 2>/dev/null && { kill -9 "$pid" 2>/dev/null; }
      ;;
    esac
  done
}

wait_ready() {
  local port="$1" i
  for i in $(seq 1 90); do
    curl -fsS "http://127.0.0.1:$port/v1/models" >/dev/null 2>&1 && return 0
    sleep 2
  done
  return 1
}

MODEL="Qwen/Qwen2.5-Omni-7B"
case "${1:-instruct}" in
  omni)
    [ "${2:-}" != "" ] && MODEL="$2"
    echo "== switching to vLLM-Omni (:8091, $MODEL) =="
    reap llama 8081                                   # free VRAM (no umount of shared cache)
    "$SUDO" stop llama-cuda                           # reset the unit so it is not 'active'
    "$RESTART_OMNI" start "$MODEL"
    exit $?
    ;;
  instruct)
    echo "== switching to llama.cpp (:8081, ornith) =="
    reap api_server 8091                              # free VRAM from vLLM-Omni
    "$RESTART_OMNI" stop
    "$SUDO" start llama-cuda                          # relaunch the llama backend on :8081
    if wait_ready 8081; then
      echo "switch: llama.cpp backend healthy on :8081."
    else
      echo "switch: llama.cpp not healthy on :8081." >&2; exit 1
    fi
    ;;
  *)
    echo "usage: $0 {omni|instruct} [MODEL]" >&2; exit 2 ;;
esac
