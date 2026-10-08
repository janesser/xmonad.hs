#!/bin/bash
# pi.sh — launch pi with the session that matches the current project.
#
# Inspects the sessions stored for the current working directory:
#   * 0 or 1 session  ->  `pi --continue`   (unambiguous; creates one if absent)
#   * more than 1     ->  list them and let you pick one, then
#                         `pi --session-id <chosen>`
#
# The session id is read from the session file path (authoritative, free, no
# API call, no mutation). It is scoped to the project cwd, so it only ever
# lists sessions for *this* project.
#
# Env hatches (for scripts/testing):
#   PI_SH_DRY_RUN=1   print the command instead of exec'ing pi
#   PI_CHOOSE=<n>     auto-select the 1-based nth session without prompting
#
# Usage:
#   pi.sh [cwd]                  resolve and launch pi (default cwd: $PWD)
#   pi.sh --list [cwd]           only list the candidate sessions, don't launch
#   pi.sh --dry-run [cwd]        print the pi command(s) that would run
#
set -euo pipefail

SESSIONS="${PI_SESSIONS_DIR:-${HOME:-$HOME}/.pi/agent/sessions}"

# Encode a cwd the same way pi encodes it in the session dir name:
# strip the leading '/', then replace '/' and ':' with '-'.
_encode_cwd() {
    local c="$1"
    c="${c#/}"
    c="${c//\//-}"
    c="${c/:/-}"
    printf -- --%s-- "$c"
}

# Collect 'date|id' rows for every session file of a project, newest first.
_rows() {
    local encoded epoch fname id date
    encoded="$(_encode_cwd "$1")"
    find "$SESSIONS/$encoded" -maxdepth 1 -type f -name '*.jsonl' 2>/dev/null \
        -printf '%T@ %f\n' | sort -rn | while read -r epoch fname; do
            id=$(basename "$fname" \
                | sed -E 's#.*_([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.jsonl#\1#')
            [[ -z "$id" ]] && continue
            date=$(date -d "@$epoch" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "n/a")
            printf '%s|%s\n' "$date" "$id"
        done
}

# Print the pi command for a chosen 1-based index.
_command_for() {
    local idx=$(( $1 - 1 )) row
    row="${rows[$idx]}"
    echo "pi --session-id ${row#*|}"
}

main() {
    local action="launch" cwd="$PWD"
    case "${1:-}" in
        --list)    action="list";    [[ -n "${2:-}" ]] && cwd="$2" ;;
        --dry-run) action="dryrun";  [[ -n "${2:-}" ]] && cwd="$2" ;;
        *)         [[ -n "${1:-}" ]] && cwd="$1" ;;
    esac

    # Env hatch: force dry-run over the interactive launch path.
    if [[ $action == launch && -n "${PI_SH_DRY_RUN:-}" ]]; then
        action="dryrun"
    fi

    local -a rows=()
    mapfile -t rows < <(_rows "$cwd")
    local n=${#rows[@]}

    case "$action" in
        list)
            if [[ $n -eq 0 ]]; then
                echo "pi.sh: no pi session for project: $cwd" >&2
                exit 1
            fi
            printf '%s session(s) for %s:\n' "$n" "$cwd"
            local i=1 d id
            for row in "${rows[@]}"; do
                d="${row%%|*}"; id="${row#*|}"
                printf '  %d  %s  %s\n' "$i" "$d" "$id"
                ((i++))
            done
            return 0
            ;;
    esac

    # 0 or 1 session -> `pi --continue` (creates one if absent).
    if [[ $n -le 1 ]]; then
        if [[ "$action" == dryrun ]]; then
            echo "pi --continue"
            return 0
        fi
        exec pi --continue
    fi

    # More than one: dry-run previews every candidate; launch prompts to choose.
    printf '%s session(s) for %s:\n' "$n" "$cwd"
    local i=1 d id
    for row in "${rows[@]}"; do
        d="${row%%|*}"; id="${row#*|}"
        printf '  %d  %s  %s\n' "$i" "$d" "$id"
        ((i++))
    done

    if [[ "$action" == dryrun ]]; then
        # A chosen index wins over the full preview.
        if [[ -n "${PI_CHOOSE:-}" ]]; then
            if [[ "$PI_CHOOSE" != *[0-9]* || PI_CHOOSE -lt 1 || PI_CHOOSE -gt $n ]]; then
                echo "pi.sh: PI_CHOOSE ${PI_CHOOSE} out of range (1-$n)" >&2
                exit 2
            fi
            _command_for "$PI_CHOOSE"
        else
            for row in "${rows[@]}"; do
                echo "pi --session-id ${row#*|}"
            done
        fi
        return 0
    fi

    # Interactive choose.
    local choice chosen=""
    if [[ -n "${PI_CHOOSE:-}" ]]; then
        choice="$PI_CHOOSE"
    else
        choice=""
        while true; do
            printf 'Choose session [1-%s] (q to cancel): ' "$n"
            if ! read -r choice; then choice="q"; fi
            case "$choice" in
                q|Q|cancel|'')
                    echo "cancelled" >&2; exit 1 ;;
                *[!0-9]*)
                    echo "please enter a number between 1 and $n (or q to cancel)" >&2
                    continue ;;
            esac
            if [[ "$choice" -ge 1 && "$choice" -le "$n" ]]; then
                break
            fi
            echo "out of range (1-$n)" >&2
        done
    fi

    local idx=$(( choice - 1 ))
    chosen="${rows[$idx]#*|}"
    exec pi --session-id "$chosen"
}

main "$@"
