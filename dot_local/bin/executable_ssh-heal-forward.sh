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
# Because this runs in its own shell it cannot rewrite the parent's environment
# — there is deliberately no eval step. Instead it heals and then execs a
# command, so the healed SSH_AUTH_SOCK is simply in effect for that command:
#
# Usage:
#   ssh-heal-forward.sh            # heal, then run ssh-add -l to show the agent
#   ssh-heal-forward.sh ssh host   # heal, then run this command
#   ssh-heal-forward.sh git fetch  # ...with SSH_AUTH_SOCK pointed at a live agent
#
# Behaviour:
#   - If the current SSH_AUTH_SOCK already answers, it is left as-is.
#   - Otherwise ~/.ssh/agent is scanned for a live s.*.sshd.* agent.
#   - With no command, `ssh-add -l` is run to report the live agent.
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

main() {
    local live
    if ! live="$(find_live_agent)"; then
        printf "ssh-heal-forward: no live forwarding agent in %s\n" "$agent_dir" >&2
        return 1
    fi
    export SSH_AUTH_SOCK="$live"

    if [ "$#" -eq 0 ]; then
        exec ssh-add -l
    fi
    exec "$@"
}

main "$@"
