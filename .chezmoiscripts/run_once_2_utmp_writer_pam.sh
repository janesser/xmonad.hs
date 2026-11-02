#!/bin/bash
# run_once_2_utmp_writer_pam.sh
#
# Configure the utmp-writer (janesser/utmp-writer) PAM trigger.
#
#   /usr/local/bin/utmp-writer must be installed first (make install, or the
#   system image already provides it). This script only wires the PAM line that
#   makes `wall`, `who` and `write` work while sessions run — the part a fresh
#   build of `/run/utmp` alone cannot do.
#
#   source: .chezmoiscripts/run_once_2_utmp_writer_pam.sh (this file)
#   target: /etc/pam.d/common-session (append line)

set -euo pipefail

PAM_LINE="session optional    pam_exec.so /usr/local/bin/utmp-writer"
DEST="/etc/pam.d/common-session"

# Idempotent: only writes when the line is missing.
if grep -q "pam_exec.so /usr/local/bin/utmp-writer" "${DEST}" 2>/dev/null; then
    echo "===> utmp-writer: PAM line already in ${DEST}"
    exit 0
fi

echo "===> utmp-writer: appending PAM line to ${DEST}"
sudo tee -a "${DEST}" >/dev/null <<EOF
${PAM_LINE}
EOF
echo "   added: ${PAM_LINE}"

# Also enable the boot-time sync service if the package is installed.
if [ -f "/usr/local/lib/systemd/system/utmp-writer.service" ]; then
    echo "===> utmp-writer: enabling systemd service"
    sudo systemctl enable --now utmp-writer >/dev/null 2>&1 || true
fi

echo "===> utmp-writer: done"
