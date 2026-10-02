#!/bin/bash
# executable_update-localai-hf-sources.sh
#
# Expose the GGUF models that already live in the HuggingFace hub cache to
# LocalAI. LocalAI's loader does not read the transformers-format
# ~/.cache/huggingface/hub tree directly, so each served model is wired with
# two things in its models dir:
#
#   1. a symlink  <models>/<physical-link>.gguf  -> <abs path in the HF cache>
#   2. a sidecar  <models>/<sidecar>.gguf.yaml    -> LocalAI model config
#
# This script (re)creates both, idempotently. It does NOT pull models — the HF
# cache is assumed populated (it is bind-mounted from passeport). Reproducible
# on a fresh box once the cache has been mirrored/pulled: run this after the
# cache is present. Point PHYSICAL_PATH at stable HF refs (blobs/ or a
# canonical models--<repo>/ snapshot), never a tmp/ dir.
#
# Sourced/run directly; no sudo needed (all paths are user-owned).

set -euo pipefail

HF_CACHE="${HF_CACHE:-$HOME/.cache/huggingface/hub}"
LOCALAI_HOME="${LOCALAI_HOME:-$HOME/.local/share/localai}"
MODELS_DIR="$LOCALAI_HOME/models"

log() { printf '%s\n' "$*"; }
note() { printf '  %s\n' "$*"; }

# --- manifest ---------------------------------------------------------------
# One entry per served model, fields separated by '|':
#   sidecar | physical-link | physical-path | backend | f16 |
#   gpu_layers | context_size | threads | enabled
#
#   sidecar         : LocalAI model name (e.g. qwen-sycl); sidecar written as
#                     <name>.gguf.yaml
#   physical-link   : basename of the symlink in models/ (what parameters.model
#                     points at)
#   physical-path   : absolute path to the GGUF inside the HF cache (stable ref)
#   backend         : llama-cpp (GPU) or cpu-llama-cpp (CPU)
#
# qwen-sycl and qwen-05b share ONE physical link (qwen25.gguf) but get two
# sidecars with different YAML schemas (GPU: top-level context_size+f16; CPU:
# context_size under parameters:). The physical link is created once.
MODELS=(
  "qwen-sycl|qwen25.gguf|$HF_CACHE/models--Qwen--qwen2.5-0.5b-instruct-GGUF/qwen2.5-0.5b-instruct-q4_k_m.gguf|llama-cpp|true|999|32768|4|true"
  "qwen-05b|qwen25.gguf|$HF_CACHE/models--Qwen--qwen2.5-0.5b-instruct-GGUF/qwen2.5-0.5b-instruct-q4_k_m.gguf|cpu-llama-cpp|false|0|2048|4|true"
  "antares-1b|antares-1b.gguf|$HF_CACHE/models--DevQuasar--fdtn-ai.antares-1b-GGUF/blobs/1f4d922bbf2d317944185ff1155feb8b9dcfa48d37617fdeaec56e9c2af54b1f|cpu-llama-cpp|false|0|2048|4|true"
)

die() { log "ERROR: $*" >&2; exit 1; }

[ -d "$HF_CACHE" ] || die "HF cache not found: $HF_CACHE (is the bind-mount present?)"
[ -d "$LOCALAI_HOME" ] || die "LocalAI home not found: $LOCALAI_HOME"
mkdir -p "$MODELS_DIR"

# Proven YAML schemas. GPU backends read context_size + f16 as TOP-LEVEL model
# fields (under parameters: they are ignored by llama-cpp, which maps them to
# --params). CPU backends keep context_size under parameters: as before.
emit_gpu_yaml() {
  cat <<EOF
name: $1
backend: llama-cpp
f16: $3
context_size: $5
parameters:
  model: $2
  gpu_layers: $4
  threads: $6
mmap: false
enabled: $7
EOF
}

emit_cpu_yaml() {
  cat <<EOF
name: $1
backend: cpu-llama-cpp
parameters:
  model: $2
  context_size: $5
  threads: $6
  gpu_layers: $4
mmap: false
enabled: $7
EOF
}

declare -A LINK_SEEN=()
entries=0
for entry in "${MODELS[@]}"; do
  IFS='|' read -r name physlink physpath backend f16 gpu_layers ctx threads en <<<"$entry"

  PHYS="$physpath"
  [ -f "$PHYS" ] || die "physical GGUF missing: $PHYS"

  link="$MODELS_DIR/$physlink"
  sidecar="$MODELS_DIR/$name.gguf.yaml"

  # 1. physical symlink (create once even when shared by two sidecars)
  if [ -n "${LINK_SEEN[$physlink]:-}" ]; then
    note "[link] $physlink : shared, already created — skipped"
  elif [ -L "$link" ] && [ "$(readlink -f "$link")" = "$PHYS" ]; then
    note "[link] $physlink : OK (-> $PHYS)"
  else
    ln -sfn "$PHYS" "$link"
    note "[link] $physlink : created -> $PHYS"
  fi
  LINK_SEEN[$physlink]=1

  # 2. sidecar (atomic write; keep the exact proven schema per backend)
  tmp="$(mktemp)"
  if [ "$backend" = "cpu-llama-cpp" ]; then
    emit_cpu_yaml "$name" "$physlink" "$f16" "$gpu_layers" "$ctx" "$threads" "$en" >"$tmp"
  else
    emit_gpu_yaml "$name" "$physlink" "$f16" "$gpu_layers" "$ctx" "$threads" "$en" >"$tmp"
  fi
  if [ -f "$sidecar" ] && diff -q "$tmp" "$sidecar" >/dev/null; then
    note "[yaml] $name : unchanged"
  else
    mv -f "$tmp" "$sidecar"
    note "[yaml] $name : written"
  fi
  rm -f "$tmp"
  entries=$((entries + 1))
done

log ""
log "✅ LocalAI HF sources updated in $MODELS_DIR ($entries models)."
log "Restart the localai user unit to load the wiring:"
log "  systemctl --user restart localai"
