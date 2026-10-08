#!/bin/bash
# pi.sh — launch pi for the current project, deciding the session deterministically.
#
# Reads the project's saved sessions from ~/.pi/agent/sessions/--<cwd>--/ (the
# source of truth; it survives reboot, unlike /tmp or ~/.cache). Only ever reads
# file paths to learn ids: no pi API call, no mutation.
#
# Modes:
#   pi.sh [cwd]          launch (cwd defaults to $PWD):
#                          >=1 known session -> ask which to resume, or start new
#                          (never auto-resumes; prompts only when interactive)
#                          0 known sessions       -> start a new session
#   pi.sh --list  [cwd]  list the known sessions (observation only)
#   pi.sh --current [cwd] print the newest-known session id (observation only)
#   pi.sh --new   [cwd]  launch a brand-new session (never resume)
#   --dry-run            print the pi command instead of launching it
#
set -euo pipefail

SESSIONS="${PI_SESSIONS_DIR:-${HOME:-$HOME}/.pi/agent/sessions}"

# Encode a cwd the way pi names its session dir: strip the leading '/', then
# replace '/' and ':' with '-'.
_encode_cwd() {
    local c="$1"
    c="${c#/}"
    c="${c//\//-}"
    c="${c/:/-}"
    printf -- --%s-- "$c"
}

# Newest-first rows of 'date|id' for a project.
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

# The newest-known session id for a project (exit 1 if there is none).
current_id() {
    local -a rows=()
    mapfile -t rows < <(_rows "$1")
    [[ ${#rows[@]} -eq 0 ]] && return 1
    printf '%s\n' "${rows[0]#*|}"
}

# A brand-new session id, so `pi --session-id` creates a fresh session.
new_id() {
    if command -v uuidgen >/dev/null 2>&1; then
        uuidgen
    elif cat /proc/sys/kernel/random/uuid >/dev/null 2>&1; then
        cat /proc/sys/kernel/random/uuid
    else
        printf 'pi-new-%s' "$(date +%Y%m%d%H%M%S)"
    fi
}

# Print the numbered known sessions. Exit 1 if there are none.
list_sessions() {
    local cwd="$1"
    local -a rows=()
    mapfile -t rows < <(_rows "$cwd")
    local n=${#rows[@]} i
    if [[ $n -eq 0 ]]; then
        echo "pi.sh: no pi session for project: $cwd" >&2
        return 1
    fi
    printf '%s session(s) for %s:\n' "$n" "$cwd"
    i=1
    local row d id
    for row in "${rows[@]}"; do
        d="${row%%|*}"; id="${row#*|}"
        printf '  %d  %s  %s\n' "$i" "$d" "$id"
        i=$((i + 1))
    done
}

# Launch a brand-new session for the current project.
launch_new() {
    local cwd="$1" cmd
    cmd="pi --session-id $(new_id)"
    if [[ $DRY_RUN -eq 1 ]]; then echo "$cmd"
    else exec $cmd; fi
}

# Interactive ask: choose a known session, or start new. Never auto-resumes.
ask_launch() {
    local cwd="$1"
    local -a rows=()
    mapfile -t rows < <(_rows "$cwd")
    local n=${#rows[@]}
    list_sessions "$cwd"

    # Nothing known -> only a new session is possible.
    if [[ $n -eq 0 ]]; then
        launch_new "$cwd"
        return 0
    fi

    # Ask only when a human can answer; at a post-reboot hook stdin is not a TTY.
    if [[ ! -t 0 ]]; then
        echo "pi.sh: $n session(s) for $cwd (none selected) - re-run 'pi.sh' interactively" >&2
        return 1
    fi

    local choice row idx chosen cmd=""
    while true; do
        printf 'resume [1-%s], or "new"? ' "$n"
        if ! read -r choice; then choice=""; fi
        case "$choice" in
            ""|q|Q|cancel)
                echo "cancelled" >&2; return 1 ;;
            new|n|N)
                cmd="pi --session-id $(new_id)"; break ;;
            *[!0-9]*)
                echo "enter 1-$n, or 'new'" >&2; continue ;;
        esac
        idx=$((choice - 1))
        if [[ $idx -ge 0 && $idx -lt $n ]]; then
            row="${rows[$idx]}"
            cmd="pi --session-id ${row#*|}"
            break
        fi
        echo "out of range (1-$n)" >&2
    done

    if [[ $DRY_RUN -eq 1 ]]; then echo "$cmd"
    else exec $cmd; fi
}

main() {
    DRY_RUN=0
    local filtered=() a
    for a in "$@"; do
        if [[ "$a" == "--dry-run" ]]; then DRY_RUN=1
        else filtered+=("$a"); fi
    done

    local action="ask" cwd="$PWD"
    if [[ ${#filtered[@]} -gt 0 ]]; then
        case "${filtered[0]}" in
            --list)    action="list" ;;
            --current) action="current" ;;
            --new)     action="new" ;;
            *)         action="ask" ;;
        esac
        [[ ${#filtered[@]} -ge 2 ]] && cwd="${filtered[1]}"
    fi

    case "$action" in
        list)     list_sessions "$cwd" ;;
        current)  current_id "$cwd" ;;
        new)      launch_new "$cwd" ;;
        *)        ask_launch "$cwd" ;;
    esac
}

main "$@"
