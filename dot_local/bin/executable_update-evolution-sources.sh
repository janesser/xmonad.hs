#!/bin/bash
#
# update-evolution-sources.sh
#
# Fold the live Evolution source files into the chezmoi-managed encrypted
# templates, keeping the runtime LastNotified= drift out of the repo.
#
#   1. strip LastNotified from the live sources (before hook)
#   2. re-add only the sources whose *decrypted* content actually changed
#      (LastNotified-stripped) — never re-add an unchanged file, because
#      age re-encryption is non-deterministic and would leave git noise
#   3. restore LastNotified from the sidecars (after hook)
#
# Only *.source files are touched; the hook scripts and *.lastnotified
# sidecars are never re-added.

set -uo pipefail

SOURCES_DIR="$HOME/.config/evolution/sources"

# --- locate the chezmoi repo so we can find the hook scripts ----------------
find_repo_root() {
    # 1) ask chezmoi where its source tree lives
    if command -v chezmoi >/dev/null 2>&1; then
        local r
        r=$(chezmoi data 2>/dev/null | sed -n 's/.*"sourceDir"[[:space:]]*:[[:space:]]*"\(.*\)".*/\1/p' | head -n1)
        if [[ -n "$r" && -d "$r/.git" ]]; then
            printf '%s' "$r"; return 0
        fi
    fi
    # 2) walk up from the current directory to a git work tree
    local d=$PWD
    while [[ "$d" != "/" ]]; do
        if [[ -d "$d/.git" ]]; then printf '%s' "$d"; return 0; fi
        d=$(dirname "$d")
    done
    # 3) well-known default
    if [[ -d "$HOME/.local/share/chezmoi/.git" ]]; then
        printf '%s' "$HOME/.local/share/chezmoi"; return 0
    fi
    return 1
}

if ! repo_root=$(find_repo_root); then
    echo "update-evolution-sources: could not locate the chezmoi repo (run me from the repo, or install it under ~/.local/share/chezmoi)" >&2
    exit 1
fi

hook_dir="$repo_root/dot_config/private_evolution/private_sources"
before_hook="$hook_dir/run_before_any_evolution_file_applies.sh"
after_hook="$hook_dir/run_after_z_last_file_was_applied.sh"

# --- both hooks must be present before we touch anything --------------------
for hook in "$before_hook" "$after_hook"; do
    if [[ ! -r "$hook" ]]; then
        echo "update-evolution-sources: required hook missing or unreadable: $hook" >&2
        exit 1
    fi
done

if [[ ! -d "$SOURCES_DIR" ]]; then
    echo "update-evolution-sources: sources dir not found: $SOURCES_DIR" >&2
    exit 1
fi

echo "update-evolution-sources: repo=$repo_root"

# --- 1. strip the runtime LastNotified= line from every source --------------
echo "update-evolution-sources: [1/3] stripping LastNotified (before hook)"
bash "$before_hook"

# --- 2. re-add only the sources whose decrypted content genuinely changed ---
echo "update-evolution-sources: [2/3] re-adding changed sources"
readded=0
skipped_match=0
skipped_nocat=0
while IFS= read -r -d '' f; do
    [[ "$f" == *.source ]] || continue
    if grep -q '^LastNotified=' "$f" 2>/dev/null; then
        echo "  skip (still has LastNotified): $f"
        continue
    fi
    # Compare the decrypted committed template against the live file, both
    # with the runtime LastNotified= line removed. Only re-add on a real diff.
    tmpl=$(mktemp)
    live=$(mktemp)
    chezmoi cat "$f" >"$tmpl" 2>/dev/null
    sed '/^LastNotified=/d' "$f" >"$live" 2>/dev/null
    if ! diff -q "$tmpl" "$live" >/dev/null 2>&1; then
        if command -v chezmoi >/dev/null 2>&1; then
            if chezmoi re-add "$f"; then
                echo "  re-added (changed): $f"
                readded=$((readded + 1))
            else
                echo "  warn: chezmoi re-add failed for $f" >&2
            fi
        else
            echo "  warn: chezmoi not found on PATH; skipping $f" >&2
        fi
    else
        skipped_match=$((skipped_match + 1))
    fi
    rm -f "$tmpl" "$live"
done < <(find "$SOURCES_DIR" -type f -print0)
echo "  re-added: $readded, unchanged-skipped: $skipped_match"

# --- 3. restore LastNotified from the sidecars ------------------------------
echo "update-evolution-sources: [3/3] restoring LastNotified (after hook)"
bash "$after_hook"

echo "update-evolution-sources: done"
