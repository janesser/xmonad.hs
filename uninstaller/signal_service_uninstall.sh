#!/bin/bash
# uninstaller/signal_service_uninstall.sh
#
# Removes the stale /etc/systemd/system/<unit>.service files on boxes where a
# system-scope unit was replaced by a user-scope one. All four live in the repo
# now, rendered to ~/.config/systemd/user/*.service:
#
#   signal-receive.service     -> dot_config/systemd/user/signal-receive.service
#   pi-memory-hub.service      -> dot_config/systemd/user/pi-memory-hub.service
#   llama-little-brother.service -> dot_config/systemd/user/llama-little-brother.service
#   llama-cuda.service           -> dot_config/systemd/user/llama-cuda.service
#
# They were migrated to user scope, but the old system-scope files were never
# removed. The signal one is still enabled + running on boxes where
# run_once_5_aitools_8signal_pi.sh never ran (hostname gate for raspberrypi) — it
# uses the old root-owned /usr/local/bin/signal-receive-loop.sh (port 8080,
# localhost only) instead of all interfaces on :8081 like the user-space unit
# does. The hub one is a leftover of the pre-user-scope setup: it still holds port
# 8083, which makes the user-space hub daemon refuse to bind. The little brother is
# the second text backend (:8082) — its system unit was replaced by jan's user-scope
# unit in this session (run_once_5_aitools_2llama_startup.sh).
#
# Only one signal-cli daemon may run with a given account: the key material in
# ~/.local/share/signal-cli/data lives on one box, so a second instance desyncs /
# duplicates messages (see discoveries/ai-server/signal-cli.md). A second hub
# instance would also fight for the same port.
#
# Idempotent — safe to run on every apply. `systemctl` steps run unattended (sudo -n,
# allowed by the base sudoers drop-in); removing the unit files needs a
# password-validated root step, which is prompted for once if a terminal is available
# (no TTY: the units stay stopped until jan removes them).
#
# This also cleans up one system-to-userspace artifact that the signal install scripts
# leave behind on boxes where the migration was interrupted: the root-owned
# /usr/local/bin/signal-cli symlink still points at an old /opt copy. The uninstaller
# is a second line of defence — it can only help, and it never fails when nothing exists.

set -euo pipefail

say() { printf "%b%s%b\n" "$(printf '\033[0;34m')" "$1" "$(printf '\033[0m')"; }

if ! [ -f /etc/systemd/system/signal-receive.service ] && \
   ! [ -f /etc/systemd/system/pi-memory-hub.service ] && \
   ! [ -f /etc/systemd/system/llama-little-brother.service ] && \
   ! [ -f /etc/systemd/system/llama-cuda.service ]; then
    say "no stale system-scope unit file left on $(hostname) -- nothing to remove."
    exit 0
fi

say "Removing stale /etc/systemd/system/ units on $(hostname) ..."

# Disable + stop first, so the daemons cannot restart between the two calls.
for unit in signal-receive.service pi-memory-hub.service llama-little-brother.service llama-cuda.service; do
    sudo -n systemctl disable --now "${unit}" 2>/dev/null || true
done
# Prompt once (only when a terminal is available) and cache the credentials, so the
# root step below does not prompt again.
sudo -v 2>/dev/null || true
for unit in signal-receive.service pi-memory-hub.service llama-little-brother.service llama-cuda.service; do
    sudo rm -f "/etc/systemd/system/${unit}" 2>/dev/null || \
        say "cannot remove /etc/systemd/system/${unit} (needs a terminal to authorize root)"
done
sudo -n systemctl daemon-reload 2>/dev/null || true

# The system-to-userspace migration of signal-cli: the stale root-owned symlink that
# still points at an /opt copy (a `rm -f` guard keeps this idempotent).
if [ -L /usr/local/bin/signal-cli ]; then
    sudo rm -f /usr/local/bin/signal-cli || \
        say "cannot remove /usr/local/bin/signal-cli (needs a terminal to authorize root)"
else
    say "no stale /usr/local/bin/signal-cli left -- nothing to remove."
fi

say "OK: stale /etc/systemd/system/ units removed."
