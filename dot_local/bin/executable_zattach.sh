#!/bin/bash
# zattach -- resume the persistent zellij session on cyberkleiber.
#
# Attaches to the single named zellij session ("persistent"), creating it on
# the first run. The session is serialized to disk so it survives disconnects
# and is resurrected across reboots (see ~/.config/zellij/config.kdl ->
# session_serialization true). Safe to call repeatedly: if the session already
# exists you just attach to it.
#
# Usage:
#   zattach        attach to (or create) the persistent session
#   zattach new    kill the persistent session and start it fresh
#   zattach list   list existing zellij sessions
set -euo pipefail

SESSION="persistent"

case "${1:-attach}" in
    attach)
        exec zellij attach -c "$SESSION"
        ;;
    new)
        if zellij list-sessions -s 2>/dev/null | grep -qx "$SESSION"; then
            zellij kill-session "$SESSION"
        fi
        exec zellij attach -c "$SESSION"
        ;;
    list)
        zellij list-sessions -s
        ;;
    *)
        echo "usage: $0 [attach|new|list]" >&2
        exit 2
        ;;
esac
