#!/bin/bash

if command -v journalctl-vacuum.sh; then
    journalctl-vacuum.sh
else
    echo 'journalctl-vacuum.sh' NOT yet installed 1
fi
