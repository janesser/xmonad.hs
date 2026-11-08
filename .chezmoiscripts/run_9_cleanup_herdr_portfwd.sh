#!/bin/bash
#
# Run hook — removes herdr-portfwd (the miko-misa/herdr-portfwd plugin and its
# CLI) from any box that got it installed. The uninstaller lives in the repo's
# `uninstaller/` tree, so `chezmoi apply` propagates it to every box: this hook
# gates itself on herdr being present and skips the rest silently.
#
# Runs on every box (like the other run_9_cleanup_* hooks); the herdr gate
# keeps it a no-op on boxes without herdr.

set -euo pipefail

command -v herdr >/dev/null || exit 0

bash "$CHEZMOI_SOURCE_DIR/uninstaller/herdr_portfwd_uninstall.sh"
