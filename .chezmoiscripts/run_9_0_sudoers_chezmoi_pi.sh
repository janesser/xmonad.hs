#!/bin/bash
# run_9_0_sudoers_chezmoi_pi.sh
#
# Install the scoped sudoers drop-in that lets pi-agent run `cz update`
# passwordlessly (see AGENTS.md for the sudo boundary).
#
#   source: etc/sudoers.d/chezmoi-pi     (NOT applied by chezmoi; etc/ is ignored)
#   target: /etc/sudoers.d/chezmoi-pi     (root:root, mode 0440)
#
# Idempotent: only acts when the file is missing or has changed.
# Consent : asks for a yes/no confirmation before writing anything under /etc.

set -euo pipefail

DEST=/etc/sudoers.d/chezmoi-pi
SRC="${CHEZMOI_SOURCE_DIR:-.}/etc/sudoers.d/chezmoi-pi"

if [ ! -f "${SRC}" ]; then
    echo "⚠️  Source sudoers file not found: ${SRC}"
    echo "   Create it, then run cz update again."
    exit 1
fi

# Idempotent: already installed and unchanged -> nothing to do.
if [ -f "${DEST}" ] && sudo cmp -s "${SRC}" "${DEST}"; then
    echo "✅ ${DEST} already installed and up to date. Nothing to do."
    exit 0
fi

# Consent gate: never prompt without a TTY (fail closed for unattended runs).
if [ ! -t 0 ]; then
    echo "⚠️  No interactive terminal — cannot ask for consent."
    echo "   Install manually:"
    echo "     sudo install -o root -g root -m 0440 etc/sudoers.d/chezmoi-pi /etc/sudoers.d/chezmoi-pi"
    echo "     sudo visudo -cf /etc/sudoers.d/chezmoi-pi"
    exit 1
fi

echo "Installing a passwordless-sudo drop-in scoped to chezmoi commands."
echo "  source: ${SRC}"
echo "  target: ${DEST}  (root:root, mode 0440)"
read -r -p "Continue? [y/N] " answer || true
case "${answer}" in
    [Yy]|[Yy][Ee][Ss]) : ;;
    *) echo "Aborted (no confirmation)."; exit 0 ;;
esac

sudo install -o root -g root -m 0440 "${SRC}" "${DEST}"
sudo visudo -cf "${DEST}"

echo "✅ ${DEST} installed and validated."
echo "Verify with: sudo -l"
