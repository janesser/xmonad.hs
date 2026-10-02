#!/bin/bash
# run_9_cleanup_nouveau_temp_sudoers.sh
#
# Remove the temporary sudoers drop-in that was added 2026-10-03 to grant
# passwordless sudo for `modprobe`/`insmod` while testing the GT730-on-nouveau
# dual-driver setup. That setup is now durable
# (run_once_5_aitools_6nouveau_gt730_driver.sh), so this drop-in must never
# persist — a passwordless kernel-module sudo rule is a security debt.
# Idempotent: a no-op where it was never installed.

set -euo pipefail

sudo rm -f /etc/sudoers.d/10-temp-nouveau-test
echo "$(basename "$0"): removed temporary sudoers drop-in /etc/sudoers.d/10-temp-nouveau-test."
