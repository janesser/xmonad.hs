#!/bin/bash
# pi.sh — launch pi for the current project, deciding the session deterministically.
#
# Reads the project's saved sessions from ~/.pi/agent/sessions/--<cwd>--/ (the
# source of truth; it survives reboot, unlike /tmp or ~/.cache). Only ever reads
# file paths to learn ids: no pi API call, no existing session is mutated.
#
# Discovery labels: when a session is started under discoveries/ (new via --new
# or the interactive "new" branch, or resumed), pi.sh records which discovery it
# contributes to in a thin .discovery sidecar next to the session
# (sessions/<--cwd-->/<id>.discovery). pi only reads the .jsonl, so this sidecar
# is invisible to it and only pi.sh uses it (shown as a column in --list).
# Discovery = the first path segment under discoveries/ (subdirs group under
# their top-level discovery); the discoveries/ root itself is labelled
# "discoveries". Outside discoveries/ nothing is written (no label). Resumes
# keep their original label.
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

# Newest-first rows of 'date|id|discovery-label' for a project.
_rows() {
    local encoded epoch fname id date label
    encoded="$(_encode_cwd "$1")"
    find "$SESSIONS/$encoded" -maxdepth 1 -type f -name '*.jsonl' 2>/dev/null \
        -printf '%T@ %f\n' | sort -rn | while read -r epoch fname; do
            id=$(basename "$fname" \
                | sed -E 's#.*_([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.jsonl#\1#')
            [[ -z "$id" ]] && continue
            date=$(date -d "@$epoch" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "n/a")
            label="$(_discovery_for_cwd "$1")"
            [[ -f "$SESSIONS/$encoded/$id.discovery" ]] && label=$(cat "$SESSIONS/$encoded/$id.discovery" 2>/dev/null)
            printf '%s|%s|%s\n' "$date" "$id" "$label"
        done
}

# The newest-known session id for a project (exit 1 if there is none).
current_id() {
    local -a rows=()
    mapfile -t rows < <(_rows "$1")
    [[ ${#rows[@]} -eq 0 ]] && return 1
    local row id
    row="${rows[0]}"                       # date|id|label
    id="${row#*|}"                         # id|label
    id="${id%%|*}"                         # id
    printf '%s\n' "$id"
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

# The discovery a cwd contributes to, or "" when outside discoveries/:
#   .../discoveries/              -> "discoveries"          (the group)
#   .../discoveries/<name>        -> "<name>"
#   .../discoveries/<name>/...    -> "<name>"  (subdirs group under their top-level discovery)
_discovery_for_cwd() {
    local c="$1" rest disp
    case "$c" in
        */discoveries/*)
            rest="${c#*/discoveries/}"   # drop everything up to 'discoveries/'
            disp="${rest%%/*}"
            [[ -n "$disp" ]] && printf '%s\n' "$disp" ;;
        */discoveries)
            printf 'discoveries\n' ;;
    esac
}

# Record, in a .discovery sidecar next to the session, which discovery it
# belongs to. Non-invasive: pi only reads the .jsonl; pi.sh reads the sidecar.
# No label outside discoveries/ => nothing is written. Preserves an existing
# label (a session's cwd is fixed, so it is never relabelled).
label_session() {
    local cwd="$1" sid="$2" label side
    label="$(_discovery_for_cwd "$cwd")"
    [[ -z "$label" ]] && return 0
    side="$SESSIONS/$(_encode_cwd "$cwd")/$sid.discovery"
    if [[ ! -e "$side" ]]; then
        # The session dir does not exist until pi creates it; make sure of it.
        mkdir -p "$(dirname "$side")"
        printf '%s\n' "$label" > "$side"
    fi
}

# Human note about the discovery a cwd (or session) carries — used by --dry-run.
_show_discovery() {
    local cwd="$1" label
    label="$(_discovery_for_cwd "$cwd")"
    if [[ -n "$label" ]]; then
        echo "pi.sh: session labelled for discovery: $label  ($cwd)"
    else
        echo "pi.sh: no discovery label ($cwd)"
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
    local row d id label
    for row in "${rows[@]}"; do
        d="${row%%|*}"
        id="${row#*|}"; id="${id%%|*}"      # date|id|label -> middle field
        label="${row##*|}"
        if [[ -n "$label" ]]; then
            printf '  %d  %s  %-26s %s\n' "$i" "$d" "discovery:$label" "$id"
        else
            printf '  %d  %s  %-26s %s\n' "$i" "$d" "-" "$id"
        fi
        i=$((i + 1))
    done
}

# Launch a brand-new session for the current project.
launch_new() {
    local cwd="$1" id cmd
    id=$(new_id)
    if [[ $DRY_RUN -eq 1 ]]; then
        _show_discovery "$cwd"
        echo "pi --session-id $id"
    else
        label_session "$cwd" "$id"     # record the label before we exec pi
        exec pi --session-id "$id"
    fi
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

    local choice row idx cmd="" sid=""
    while true; do
        printf 'resume [1-%s], or "new"? ' "$n"
        if ! read -r choice; then choice=""; fi
        case "$choice" in
            ""|q|Q|cancel)
                echo "cancelled" >&2; return 1 ;;
            new|n|N)
                sid=$(new_id)                       # generate first
                cmd="pi --session-id $sid"; break ;;
            *[!0-9]*)
                echo "enter 1-$n, or 'new'" >&2; continue ;;
        esac
        idx=$((choice - 1))
        if [[ $idx -ge 0 && $idx -lt $n ]]; then
            row="${rows[$idx]}"
            sid="${row#*|}"                      # date|id|label -> id
            sid="${sid%%|*}"
            cmd="pi --session-id $sid"; break
        fi
        echo "out of range (1-$n)" >&2
    done

    if [[ $DRY_RUN -eq 1 ]]; then echo "$cmd"
    else
        label_session "$cwd" "$sid"            # label on launch (new or resume)
        exec pi --session-id "$sid"
    fi
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
