#!/usr/bin/env bash
#
# ssh-heal-forward.sh — repair a stale SSH_AUTH_SOCK by pointing it at a live
# sshd forwarding agent, then run a command against it.
#
# The sshd forwarding agent is ephemeral: each agent-forwarded connection gets
# its own socket under ~/.ssh/agent (s.<id>.sshd.<id>), and the previous one is
# removed when the connection dies. A long-lived shell, or a frozen inherited
# env var, can keep pointing at a socket that no longer exists, so every ssh /
# git / scp fails with "Error connecting to agent". This script probes the
# sockets, finds one that answers, and runs your command against it.
#
# Usage:
#   ssh-heal-forward.sh            # heal, then run ssh-add -l to show the live agent
#   ssh-heal-forward.sh <cmd>      # heal + exec a command with healed SSH_AUTH_SOCK
#   ssh-heal-forward.sh --which    # print the live agent path (for eval-based one-liner)
#   ssh-heal-forward.sh --emit     # detect parent shell and emit export/set-gx
#
# Modes --which/--emit are for the eval-based one-liner:
#     eval "$(ssh-heal-forward.sh --emit)"
# In an interactive FISH shell you do not need eval at all — use the
# ~/.config/fish/functions/ssh-heal-forward.fish function instead, which heals
# the calling shell directly (it runs inside the parent process).

set -u

# ${HOME:-$HOME} defeats chezmoi's $HOME substitution (same idiom as pi.sh)
agent_dir="${SSH_AGENT_DIR:-${HOME:-$HOME}/.ssh/agent}"

# A candidate counts as alive only if the agent actually answers ssh-add.
# A stale socket whose sshd peer died accepts the connection but never replies,
# so ssh-add -l blocks; bound the probe with a timeout so it counts as dead.
agent_alive() {
    [ -S "$1" ] || return 1
    SSH_AUTH_SOCK="$1" timeout 3 ssh-add -l >/dev/null 2>&1
}

# Print the path of a live agent, or return 1 if none is found (and print
# nothing on stdout).
find_live_agent() {
    if [ -n "${SSH_AUTH_SOCK:-}" ] && agent_alive "$SSH_AUTH_SOCK"; then
        printf '%s\n' "$SSH_AUTH_SOCK"
        return 0
    fi
    local candidate
    for candidate in "$agent_dir"/s.*.sshd.*; do
        [ -S "$candidate" ] || continue
        if agent_alive "$candidate"; then
            printf '%s\n' "$candidate"
            return 0
        fi
    done
    return 1
}

# Emit the correct export/set-directive for the calling shell.
export_directive() {
    local path="$1"
    if [ -n "${FISH_VERSION:-}" ]; then
        printf 'set -gx SSH_AUTH_SOCK "%s"\n' "$path"
    else
        printf 'export SSH_AUTH_SOCK="%s"\n' "$path"
    fi
}
#
main() {
    local args=()
    local mode=run

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --which) mode=which ;;
            --emit)  mode=emit  ;;
            *)       args+=("$1") ;;
        esac
        shift
    done

    if [ "$mode" = which ]; then
        find_live_agent || { echo "ssh-heal-forward: no live forwarding agent in $agent_dir" >&2; exit 1; }
        return 0
    fi

    if [ "$mode" = emit ]; then
        local agent
        agent="$(find_live_agent)" || { echo "ssh-heal-forward: no live forwarding agent in $agent_dir" >&2; exit 1; }
        export_directive "$agent"
        return 0
    fi

    # mode = run
    local agent
    agent="$(find_live_agent)" || { echo "ssh-heal-forward: no live forwarding agent in $agent_dir" >&2; return 1; }
    export SSH_AUTH_SOCK="$agent"

    if [ ${#args[@]} -eq 0 ]; then
        exec ssh-add -l
    fi
    exec "${args[@]}"
}
main "$@"
