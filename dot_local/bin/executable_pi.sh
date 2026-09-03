#!/bin/bash

# Update pi before launching
pi update --all
chezmoi re-add ~/.pi/agent/settings.json

# Start pi
pi "$@"
