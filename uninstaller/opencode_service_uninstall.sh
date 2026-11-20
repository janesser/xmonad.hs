#!/bin/bash

# User-level shared background service (opencode's own install path).
systemctl --user stop opencode 2>/dev/null || true
systemctl --user disable opencode 2>/dev/null || true
rm -f ~/.config/systemd/user/opencode.service

# System-level always-on unit (was deployed on raspberrypi by the former
# run_once_5_aitools_8b_opencode_pi.sh; removed 2026-11-20 in favor of
# on-demand `opencode serve` which allocates :49374 only while in use).
systemctl --system stop opencode-pi 2>/dev/null || true
systemctl --system disable opencode-pi 2>/dev/null || true
rm -f /etc/systemd/system/opencode-pi.service
systemctl --system daemon-reload 2>/dev/null || true