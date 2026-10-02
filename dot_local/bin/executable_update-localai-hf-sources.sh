#!/bin/bash
# executable_update-localai-hf-sources.sh
#
# Wire the GGUF models that currently live in the HuggingFace hub cache into
# LocalAI, and ONLY those. The cache (a bind-mount of /media/passeport's
# huggingface-hub/) is the single source of truth; there is no per-model
# manifest anymore. `hf cache list` enumerates the cached repos; this script
# picks exactly one servable GGUF per repo (smallest quant, skipping mmproj /
# non-GGUF repos like VAEs) and wires it with two things in LocalAI's models
# dir:
#
#   1. a symlink  <models>/<name>.gguf  -> the chosen GGUF (a HF cache snapshot,
#      which is itself a symlink into blobs/ — LocalAI follows it fine)
#   2. a sidecar  <models>/<name>.gguf.yaml -> LocalAI model config
#
# LocalAI's loader only scans its own models dir, so the symlink indirection
# is what lets it see the cached GGUFs. It does NOT pull models and never
# touches the cache read-only. Run it after the cache is present.
#
# Reconcile: any *.gguf / *.gguf.yaml already in the models dir whose name is
# NOT produced by this run is "stale". Stale is reported; pass --prune to
# delete it. Restart the localai user unit afterwards (or pass --restart) so
# LocalAI picks up the change.
#
# Sourced/run directly; no sudo needed (all paths are user-owned).
#
#   Usage: update-localai-hf-sources.sh [--prune] [--restart]
#
#   --prune     delete sidecars/symlinks no longer in the HF cache
#   --restart   systemctl --user restart localai after wiring

set -euo pipefail

HF_CACHE="${HF_CACHE:-$HOME/.cache/huggingface/hub}"
LOCALAI_HOME="${LOCALAI_HOME:-$HOME/.local/share/localai}"
MODELS_DIR="$LOCALAI_HOME/models"
HF_HOME_PARENT="$(dirname "$HF_CACHE")"          # HF_HOME expects the dir ABOVE hub/

# --- tunables (all env-overridable) -----------------------------------------
# backend: cpu-llama-cpp (default, proven) or llama-cpp (GPU/SYCL)
LOCALAI_BACKEND="${LOCALAI_BACKEND:-cpu-llama-cpp}"
LOCALAI_CONTEXT="${LOCALAI_CONTEXT:-2048}"
LOCALAI_THREADS="${LOCALAI_THREADS:-4}"
# how to list the cache. `hf` is not installed on this box; uvx runs it
# ephemeral. Point HF_LIST_CMD at a plain `hf cache list` (or tsv) if you have
# the CLI on PATH. Kept as a word-split array so multi-word commands work.
if [ "${HF_LIST_CMD+set}" = set ]; then
  read -ra HF_LIST_ARR <<<"$HF_LIST_CMD"
else
  HF_LIST_ARR=(uvx hf cache list)
fi

PRUNE=0; RESTART=0
[ "${1:-}" = "--prune" ] && PRUNE=1
[ "${1:-}" = "--restart" ] && RESTART=1

log() { printf '%s\n' "$*"; }
note() { printf '  %s\n' "$*"; }

die() { log "ERROR: $*" >&2; exit 1; }

command -v uvx >/dev/null 2>&1 || die "uvx not found (needed for '$HF_LIST_CMD')"
[ -d "$HF_CACHE" ] || die "HF cache not found: $HF_CACHE (is the bind-mount present?)"
[ -d "$LOCALAI_HOME" ] || die "LocalAI home not found: $LOCALAI_HOME"
mkdir -p "$MODELS_DIR"

is_gguf() { head -c 4 "$1" 2>/dev/null | grep -q 'GGUF'; }

# model/Owner/Name -> models--Owner--Name
derive_dir() { local rid="$1" d; d="${rid#model/}"; printf 'models--%s' "${d//\//--}"; }

# choose one GGUF per repo: match by name (snapshots are symlinks to blobs),
# skip mmproj (vision projectors), require GGUF magic, prefer snapshots, then
# smallest file. Prints the chosen path, or nothing if the repo has none.
pick_gguf() {
  local rdir="$1" best="" bestsize="" bestsnap=-1 f snap
  while IFS= read -r f; do
    case "$f" in *mmproj*) continue ;; esac
    is_gguf "$f" || continue
    [ -e "$f" ] || continue                          # follow symlink, skip dangling
    local sz; sz="$(stat -L -c%s "$f" 2>/dev/null)" || continue
    snap=0; case "$f" in */snapshots/*) snap=1 ;; esac
    if [ -z "$best" ] \
       || { [ "$snap" -gt "$bestsnap" ]; } \
       || { [ "$snap" -eq "$bestsnap" ] && [ "$sz" -lt "$bestsize" ]; }; then
      best="$f"; bestsize="$sz"; bestsnap="$snap"
    fi
  done < <(find "$rdir" -name '*.gguf' 2>/dev/null)
  if [ -n "$best" ]; then
    printf '%s\n' "$best"
  fi
  return 0
}

# owner/name -> a flat, valid LocalAI model id
model_name() {
  local rp="$1"
  printf '%s' "$rp" \
    | tr '[:upper:]' '[:lower:]' \
    | sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//' \
    | cut -c1-60
}

emit_sidecar() { # name physlink backend context threads
  cat <<EOF
name: $1
backend: $2
parameters:
  model: $3
  context_size: $4
  threads: $5
  gpu_layers: 0
mmap: false
enabled: true
EOF
}

# --- snapshot the models dir so we can reconcile stale entries --------------
OLD_STATE="$(mktemp)"; NEW_STATE="$(mktemp)"
trap 'rm -f "$OLD_STATE" "$NEW_STATE"' EXIT
( cd "$MODELS_DIR" && find . -maxdepth 1 \( -name '*.gguf' -o -name '*.gguf.yaml' \) \
    ! -name '._*' ! -name 'gallery*' -printf '%f\n' | sort ) >"$OLD_STATE"

# --- wire one model per cached repo -----------------------------------------
HF_HOME="$HF_HOME_PARENT" "${HF_LIST_ARR[@]}" 2>/dev/null \
  | awk '/^model\// {print $1}' >"$NEW_STATE.list"

entries=0; skipped=0
while IFS= read -r rid; do
  [ -n "$rid" ] || continue
  reldir="$(derive_dir "$rid")"
  rdir="$HF_CACHE/$reldir"
  [ -d "$rdir" ] || { note "[skip] $rid : cache dir missing ($rdir)"; skipped=$((skipped+1)); continue; }

  gguf="$(pick_gguf "$rdir")"
  if [ -z "$gguf" ]; then note "[skip] $rid : no servable GGUF (non-model repo e.g. VAE/mmproj)"; skipped=$((skipped+1)); continue; fi

  name="$(model_name "${reldir#models--}")"
  [ -n "$name" ] || { note "[skip] $rid : could not derive a model name"; skipped=$((skipped+1)); continue; }

  physlink="$name.gguf"
  link="$MODELS_DIR/$physlink"
  sidecar="$MODELS_DIR/$name.gguf.yaml"
  tmp="$(mktemp)"

  # 1. physical symlink (create once, point at the chosen GGUF)
  if [ -L "$link" ] && [ "$(readlink -f "$link")" = "$gguf" ]; then
    note "[link] $physlink : OK"
  else
    ln -sfn "$gguf" "$link"
    note "[link] $physlink -> $(basename "$gguf")"
  fi

  # 2. sidecar (atomic write; proven CPU schema)
  emit_sidecar "$name" "$LOCALAI_BACKEND" "$physlink" "$LOCALAI_CONTEXT" "$LOCALAI_THREADS" >"$tmp"
  if [ -f "$sidecar" ] && diff -q "$tmp" "$sidecar" >/dev/null 2>&1; then
    note "[yaml] $name : unchanged"
  else
    mv -f "$tmp" "$sidecar"
    note "[yaml] $name : written"
  fi
  rm -f "$tmp"

  # record wired basenames for reconcile
  printf '%s\n' "$physlink" >>"$NEW_STATE"
  printf '%s\n' "$name.gguf.yaml" >>"$NEW_STATE"
  entries=$((entries+1))
done <"$NEW_STATE.list"
rm -f "$NEW_STATE.list"

# --- reconcile: report (or prune) stale sidecars/symlinks -------------------
echo ""
stale=0
while IFS= read -r oldbase; do
  [ -n "$oldbase" ] || continue
  if ! grep -qxF "$oldbase" "$NEW_STATE"; then
    stale=$((stale+1))
    if [ "$PRUNE" -eq 1 ]; then
      rm -f "$MODELS_DIR/$oldbase"
      note "[prune] removed stale $oldbase"
    else
      note "[stale]   $oldbase  (add --prune to remove)"
    fi
  fi
done <"$OLD_STATE"

log ""
log "✅ Wired $entries model(s) from the HF cache into $MODELS_DIR (skipped $skipped repos)."
[ "$stale" -gt 0 ] && log "ℹ   $stale stale sidecar(s) left in place (pass --prune to delete)."
log "Restart the localai user unit to load the wiring:"
log "  systemctl --user restart localai"
if [ "$RESTART" -eq 1 ]; then
  if systemctl --user restart localai; then
    log "  (restarted localai)"
  else
    log "  (restart returned non-zero — run 'systemctl --user restart localai' manually)"
  fi
fi
