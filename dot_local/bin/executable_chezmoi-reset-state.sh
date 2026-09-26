#!/usr/bin/env bash
#
# chezmoi-reset-state.sh — forget chezmoi's run-script state so every
# .chezmoiscripts/* script is replayed on the next apply.
#
# chezmoi records which run scripts it has already executed in the
# "scriptState" bucket of its persistent state (keyed by each script's
# SHA256, each entry carrying the last "runAt" time). That is how it
# decides to skip a script that has not changed. Deleting the whole
# scriptState bucket makes chezmoi forget everything, so the next
# `chezmoi apply` / `cz apply` / `cz update` re-runs all scripts from
# scratch — the intended way to replay a set of run scripts.
#
# This only clears run-script bookkeeping. It leaves the entry state
# (which files chezmoi manages) and every other state bucket untouched.
#
# Usage:
#   chezmoi-reset-state.sh            interactive: print plan, ask to confirm
#   chezmoi-reset-state.sh --dry-run  print the plan, change nothing
#   chezmoi-reset-state.sh --yes      skip the confirmation prompt
#   chezmoi-reset-state.sh -h         this help
#
set -euo pipefail

CHEZMOI="${CHEZMOI:-$(command -v chezmoi)}"

usage() {
    cat <<'EOF'
Forget chezmoi's run-script state so all scripts replay on next apply.

 chezmoi remembers executed run scripts in the "scriptState" bucket of
 its persistent state; deleting it makes the next apply re-run them all.

 Usage:
   chezmoi-reset-state.sh            interactive: print plan, ask to confirm
   chezmoi-reset-state.sh --dry-run  print the plan, change nothing
   chezmoi-reset-state.sh --yes      skip the confirmation prompt
   chezmoi-reset-state.sh -h         this help
EOF
}

DRY_RUN=0
ASSUME_YES=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run) DRY_RUN=1 ;;
        --yes|-y)  ASSUME_YES=1 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; usage; exit 2 ;;
    esac
    shift
done

command -v "$CHEZMOI" >/dev/null 2>&1 || {
    echo "chezmoi not found (CHEZMOI='$CHEZMOI')." >&2
    exit 127
}

# Count how many run scripts are currently remembered (0 if parse fails).
count_script_state() {
    "$CHEZMOI" state dump 2>/dev/null | python3 -c \
        'import sys,json; print(len(json.load(sys.stdin).get("scriptState",{})))' 2>/dev/null \
        || echo 0
}

BEFORE="$(count_script_state)"
echo "Run scripts currently remembered by chezmoi: $BEFORE"

if [[ "$DRY_RUN" -eq 1 ]]; then
    echo "[dry-run] Would delete the 'scriptState' bucket. Nothing changed."
    exit 0
fi

if [[ "$ASSUME_YES" -eq 0 ]]; then
    read -r -p "Delete chezmoi run-script state so ALL scripts replay on next apply? [y/N] " answer
    case "$answer" in
        [yY]|[yY][eE][sS]) ;;
        *) echo "Aborted."; exit 0 ;;
    esac
fi

"$CHEZMOI" state delete-bucket --bucket scriptState

AFTER="$(count_script_state)"
echo "Run scripts now remembered: $AFTER"
echo "Next 'chezmoi apply' / 'cz apply' / 'cz update' will re-run ALL scripts."
