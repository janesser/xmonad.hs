#!/bin/bash

# Script to set up an SSH agent using systemd user services, based on nbailey.ca instructions.
# This script should be run once to configure the environment variables and service unit files.

echo "--- Starting SSH Agent Setup ---"

# 1. Create the systemd user-level config directory
mkdir -p ~/.config/systemd/user/

# 2. Create the service unit file
SERVICE_FILE="$HOME/.config/systemd/user/ssh-agent.service"

cat > "$SERVICE_FILE" <<'EOF'
[Unit]
Description=SSH key agent

[Service]
Type=simple
Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket
ExecStart=/usr/bin/ssh-agent -D -a \$SSH_AUTH_SOCK

[Install]
WantedBy=default.target
EOF

# 3. Enable and start the unit
systemctl --user enable ssh-agent --now
