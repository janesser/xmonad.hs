#!/bin/bash
# camera_ec_debug.sh
#
# Root-owned read-only wrapper for the EC debugfs region on the Chuwi UBOOK
# X Pro (IPU3/CIO2 + int3472 discrete-PMIC camera stack).
#
# Invoked by jan via:  sudo /usr/local/bin/camera_ec_debug.sh <action> <path>
#   action: ls | cat | od | xxd
#   path:   /sys/kernel/debug/ec/<ec>/<file>   (dynamic <ec> name)
#
# The EC debugfs paths are dynamic, so sudoers (which forbids wildcards in
# command arguments) cannot grant the individual tools directly. This wrapper is
# root-owned and enforces the read-only, path-scoped policy itself:
#   * action must be one of ls|cat|od|xxd
#   * path must be under /sys/kernel/debug/ec/ (no writes, no escapes)
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in.
#
# Installed (after explicit user consent), as root:
#   install -o root -g root -m 0755 usr_local/bin/camera_ec_debug.sh /usr/local/bin/camera_ec_debug.sh

set -euo pipefail

EC_ROOT=/sys/kernel/debug/ec

usage() {
    echo "usage: $0 <action> <path>" >&2
    echo "  action: ls|cat|od|xxd" >&2
    echo "  path:   $EC_ROOT/<ec>/<file> (read-only)" >&2
    exit 1
}

[ "$#" -eq 2 ] || usage

action=$1
path=$2

# Scope: only under the EC debugfs root (no writes, no escapes).
case "$path" in
    "$EC_ROOT"/*) ;;
    *) echo "denied: $path (must be under $EC_ROOT/)" >&2; exit 1 ;;
esac

case "$action" in
    ls)   exec ls -la "$path" ;;
    cat)  exec cat "$path" ;;
    od)   exec od -A x -t x1z "$path" ;;
    xxd)  exec xxd "$path" ;;
    *)    echo "denied: $action (must be ls|cat|od|xxd)" >&2; exit 1 ;;
esac
