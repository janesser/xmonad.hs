# Managed by chezmoi.
#
# Make the shell see the live system ssh-agent.
#
# `~/.config/environment.d/ssh-agent.conf` (also managed by chezmoi) is only
# read by systemd user units and the display manager — fish ignores it. This
# snippet overrides `SSH_AUTH_SOCK` when it points to a dead agent (as it does
# when herdr's `~/.config/herdr/herdr.sock.agent` symlink is stale), so every
# `ssh`/`git`/`ssh-add` command finds a working agent.
#
# The agent socket is created by the user-level systemd service
# `ssh-agent.service` (started by `.chezmoiscripts/run_after_ssh_agent.sh`):
#   /run/user/1000/ssh-agent.socket
#
# If a different agent is active (e.g. a forwarded agent in an SSH session),
# that socket takes precedence — this snippet only overrides when the value is
# missing or dead.

set -g __fish_ssh_agent_socket /run/user/1000/ssh-agent.socket

if [ -z "$SSH_AUTH_SOCK" ]
    set -x SSH_AUTH_SOCK "$__fish_ssh_agent_socket"
else
    if ! command -v ssh-add >/dev/null 2>&1
        set -x SSH_AUTH_SOCK "$__fish_ssh_agent_socket"
    else if ! SSH_AUTH_SOCK="$SSH_AUTH_SOCK" ssh-add -l >/dev/null 2>&1
        set -x SSH_AUTH_SOCK "$__fish_ssh_agent_socket"
    end
end

set -e __fish_ssh_agent_socket
