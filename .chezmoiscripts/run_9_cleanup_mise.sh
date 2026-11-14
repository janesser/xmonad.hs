#!/bin/bash

export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)" || {
    echo "mise not on PATH; run run_once_0_mise_install.sh first"
    exit 0
}

mise prune -y

exit 0
