# Guard: fish must be present for these files to have any effect.
command -v fish >/dev/null || exit 0
# fish function: ssh-heal-forward
# Heals the calling shell's SSH_AUTH_SOCK and then runs a command.
# No eval is needed — the function runs in the parent process, so it can set
# the variable directly using fish syntax.
#
# Usage:
#   ssh-heal-forward            # heal, then run ssh-add -l to show the live agent
#   ssh-heal-forward <cmd...>   # heal + exec a command with healed SSH_AUTH_SOCK

function ssh-heal-forward --description "Repoint SSH_AUTH_SOCK to a live sshd forwarding agent, then run a command"
    # Ask the companion script to print the path of a live agent.
    set -l path
    set -l output (ssh-heal-forward.sh --which)
    if test -z "$output"
        echo "ssh-heal-forward: no live forwarding agent" >&2
        return 1
    end
    set -l path "$output"

    # Heal the parent (interactive) shell's SSH_AUTH_SOCK — no eval needed.
    set -gx SSH_AUTH_SOCK "$path"

    # With no command, show the live agent; otherwise exec the given command.
    if test (count $argv) -eq 0
        exec ssh-add -l
    end
    exec $argv
end
