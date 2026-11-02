#!/usr/bin/env bash
# executable_build-hylki.sh — build hylki (or whatever else lands in the
# lan_repo) and publish it to the Raspberry Pi.
#
#   executable_build-hylki.sh [--host <cyberkleiber|lincopta>]
#                             [--pi <pi-host>] [--keep-src] [--repo-only]
#                             [--package <aur-name>...]
#
# This is a convenience wrapper around
# executable_build-lan-repo.sh, which builds every package on the command line
# (default: hylki) in an Arch container and publishes them all to the lan_repo.
#
#   build-hylki.sh --package vscodium-bin pwvucontrol rencal-bin rptui-bin

exec "$(dirname "$0")/build-lan-repo.sh" "$@"
