#!/bin/bash
#
# Update pi-apps (the app store itself) and its apps, non-interactive.
#
# `cli` asks for confirmation before updating; `cli-yes` updates without asking
# (documented non-interactive mode: `~/pi-apps/updater cli-yes`).
#
# Non-fatal: always exits 0 so an available update never breaks `cz apply`.

set -uo pipefail

if ! command -v ~/pi-apps/updater >/dev/null 2>&1
then
    echo "==> pi-apps: ~/pi-apps/updater not found; skipping update."
    exit 0
fi

export DEBIAN_FRONTEND=noninteractive

~/pi-apps/updater cli-yes || {
    echo "==> pi-apps: updater failed (non-fatal)"
    exit 0
}

exit 0