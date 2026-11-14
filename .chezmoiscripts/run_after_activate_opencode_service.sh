#!/bin/bash

if [ "$(loginctl show-user jan --no-ask-password -p Linger --value 2>/dev/null || echo '')" != "yes" ]; then
    if loginctl enable-linger jan 2>&1; then
        echo "✅ linger enabled for jan — userspace units now start at boot"
    else
        echo "⚠️  could not enable linger for jan (userspace units need it to start at boot)"
        echo "     run: loginctl enable-linger jan"
    fi
fi

systemctl --user enable openservice --now
