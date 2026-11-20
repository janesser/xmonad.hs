#!/bin/bash
# run_after_6_reduce_surface.sh
#
# Surface reduction on cyberkleiber (hardening pass 2026-11-20):
#   - mask + stop gdm.service      — box operates headless (only a GDM greeter
#     session was observed, no user GUI seat; SSH/herdr TTY sessions unaffected)
#   - disable + stop cups-browsed  — zero CUPS print targets configured, so
#     remote printer discovery buys nothing
# Both steps are idempotent. No-op on every other box in the fleet.

set -euo pipefail

if [ "$(hostname)" != "cyberkleiber" ]; then
    echo "⏭️  $(hostname) is not cyberkleiber — surface reduction is cyberkleiber-only."
    exit 0
fi

echo "🖥️  Masking gdm.service (headless operation)…"
sudo systemctl --system mask --now gdm.service

echo "🖨️  Disabling cups-browsed.service (no print targets configured)…"
sudo systemctl --system disable --now cups-browsed.service

echo "✅ Surface reduced: gdm masked, cups-browsed disabled."
