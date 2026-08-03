#!/bin/bash
# Install udev-rules + systemd user unit for syndaemon touchpad restart

set -e

SRC="$(cd "$(dirname "$0")" && pwd)"

# --- udev rule (system-wide, root) ---
DEST_RULE="/etc/udev/rules.d/99-syndaemon-restart.rules"
if [ -f "$SRC/99-syndaemon-restart.rules" ]; then
  sudo install -m 0644 "$SRC/99-syndaemon-restart.rules" "$DEST_RULE"
  echo "Installed: $DEST_RULE"
else
  echo "SKIP: $SRC/99-syndaemon-restart.rules not found"
fi

# --- restart script (user-local) ---
DEST_BIN="$HOME/.local/bin/restart-syndaemon"
if [ -f "$SRC/restart-syndaemon" ]; then
  install -m 0755 "$SRC/restart-syndaemon" "$DEST_BIN"
  echo "Installed: $DEST_BIN"
  chezmoi re-add "$DEST_BIN"
else
  echo "SKIP: $SRC/restart-syndaemon not found"
fi

# --- systemd user unit (user-level) ---
DEST_UNIT="$HOME/.config/systemd/user/syndaemon-restart.service"
if [ -f "$SRC/syndaemon-restart.service" ]; then
  install -m 0644 "$SRC/syndaemon-restart.service" "$DEST_UNIT"
  echo "Installed: $DEST_UNIT"
  chezmoi re-add "$DEST_UNIT"
else
  echo "SKIP: $SRC/syndaemon-restart.service not found"
fi

sudo udevadm control --reload-rules
systemctl --user enable syndaemon-restart
