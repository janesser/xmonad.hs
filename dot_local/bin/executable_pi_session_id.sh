#!/bin/bash
# pi_session_id -- locate the active pi (pi-agent) conversation session id for a
# project and persist it under /tmp.
#
# The session id is the UUID embedded in the session file path:
#   ~/.pi/agent/sessions/--<cwd>--/<timestamp>_<uuid>.jsonl
# Reading it directly is authoritative, costs nothing, and mutates nothing
# (unlike `pi --print --mode json --continue`, which spends an API call and
# writes an extra turn).
#
# The id is scoped to the project cwd, and the /tmp cache file is named with
# host + user + a hash of the cwd, so it never collides across hosts, users, or
# projects.
#
# Usage:
#   pi_session_id [<cwd>]          print the id for <cwd> (default: current pwd)
#   pi_session_id --print [<cwd>]  same as above (default action)
#   pi_session_id --store [<cwd>]  write the id to the /tmp cache (default action)
#   pi_session_id --cached         print the cached id only, without any lookup
#
# Exit codes:
#   0  success
#   1  no session found for the project
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

# Collision-resistant /tmp path: host + user + sha256(cwd)[:8].
_cache_path() {
    local cwd="$1"
    local short
    short=$(printf '%s' "$cwd" | sha256sum | cut -c1-8)
    printf '/tmp/pi-session-%s-%s-%s.txt' "$(hostname)" "${USER:-$(id -un 2>/dev/null || echo unknown)}" "$short"
}

extract_id() {
    local cwd="$1" encoded cache f id
    encoded="$(_encode_cwd "$cwd")"
    cache="$(_cache_path "$cwd")"

    # Look up the most recent session file for this project.
    f=$(find "$SESSIONS/$encoded" -maxdepth 1 -type f -name '*.jsonl' 2>/dev/null | sort | tail -1)
    if [[ -z "$f" ]]; then
        echo "pi_session_id: no pi session found for project: $cwd" >&2
        exit 1
    fi

    id=$(basename "$f" | sed -E 's#.*_([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.jsonl#\1#')
    if [[ -z "$id" ]]; then
        echo "pi_session_id: could not parse session id from: $f" >&2
        exit 1
    fi

    mkdir -p "$(dirname "$cache")"
    printf '%s\n' "$id" > "$cache"
    printf '%s\n' "$id"
}

# --- argument handling -------------------------------------------------------
action="print"
case "${1:-}" in
    --print|-p) action="print"; shift ;;
    --store|-s) action="store"; shift ;;
    --cached|-c)
        action="cached"
        # An optional cwd still selects which cache file to read.
        if [[ -n "${1:-}" ]]; then shift; fi
        ;;
esac

cwd="${1:-$(pwd)}"
# Strip a cwd even if a --cached flag followed it.
[[ -z "$cwd" ]] && cwd="$(pwd)"

case "$action" in
    cached)
        cache="$(_cache_path "$cwd")"
        if [[ -s "$cache" ]]; then
            cat "$cache"
        else
            echo "pi_session_id: no cached id for: $cwd" >&2
            exit 1
        fi
        ;;
    print)
        extract_id "$cwd"
        ;;
    store)
        extract_id "$cwd" >/dev/null
        cache="$(_cache_path "$cwd")"
        echo "stored: $(cat "$cache")   ($cache)"
        ;;
esac
