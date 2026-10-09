#!/usr/bin/env bash
#
# ssh-heal-forward.sh — repair a stale SSH_AUTH_SOCK by pointing it at a live
# sshd forwarding agent.
#
# The sshd forwarding agent is ephemeral: each agent-forwarded connection gets
# its own socket under ~/.ssh/agent (s.<id>.sshd.<id>), and the previous one is
# removed when the connection dies. A long-lived shell, or a frozen inherited
# env var, can keep pointing at a socket that no longer exists, so every ssh /
# git / scp fails with "Error connecting to agent". This script probes the
# sockets, finds one that answers, and repoints.
#
# Usage:
#   eval "$(ssh-heal-forward.sh)"          # fix SSH_AUTH_SOCK in the current shell
#   ssh-heal-forward.sh ssh host           # run a command with a healed env
#   ssh-heal-forward.sh git fetch origin   # ...or any other command
#
# Behaviour:
#   - If the current SSH_AUTH_SOCK already answers, it is left as-is.
#   - Otherwise ~/.ssh/agent is scanned for a live s.*.sshd.* agent.
#   - With no command, shell-appropriate export lines are printed (for eval).
#   - With a command, it is exec'd with the healed SSH_AUTH_SOCK exported.
#
# This only reads socket files and runs ssh-add to probe them; it never
# modifies the running system or any other process.

set -u

# ${HOME:-$HOME} defeats chezmoi's $HOME substitution (same idiom as
# executable_pi.sh) so the script stays portable across hosts.
agent_dir="${SSH_AGENT_DIR:-${HOME:-$HOME}/.ssh/agent}"

# A candidate counts as alive only if the agent actually answers ssh-add
# (a dead socket lingers as a file and must be rejected).
agent_alive() {
    [ -S "$1" ] || return 1
    SSH_AUTH_SOCK="$1" ssh-add -l >/dev/null 2>&1
}

# Print the path of a live agent, or return 1 if none is found.
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

# Emit the correct export syntax for the calling shell.
emit_export() {
    if [ -n "${FISH_VERSION:-}" ]; then
        printf "set -gx SSH_AUTH_SOCK '%s'\n" "$1"
    else
        printf "export SSH_AUTH_SOCK='%s'\n" "$1"
    fi
}

main() {
    local live
    if ! live="$(find_live_agent)"; then
        printf "ssh-heal-forward: no live forwarding agent in %s\n" "$agent_dir" >&2
        return 1
    fi

    if [ "$#" -eq 0 ]; then
        emit_export "$live"
        return 0
    fi

    export SSH_AUTH_SOCK="$live"
    exec "$@"
}

main "$@"
