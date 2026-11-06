#!/bin/bash
# run_once_0_sudoers_wrappers.sh
#
# Install the root-owned sudoers wrapper script into /usr/local/bin. This is
# the only command outside of `sudo install` that this box's sudoers grants:
# it runs exactly one operation (fix ownership + mode of the podman socket
# group), which can no longer run through the agent itself.
#
# Source copy lives in the repo under usr/local/bin/ (NOT applied by chezmoi:
# usr/ is in .chezmoiignore). The `cleanup` grant of the sudoers drop-in `base`
# grants only the installed path, so a script can never be rewritten without
# changing both the source and the rule.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
for name in podman_socket_setup; do
    src="${SRC_DIR}/usr/local/bin/${name}.sh"
    dest="/usr/local/bin/${name}"
    [ -f "${src}" ] || { echo "⚠️  source not found: ${src}"; exit 1; }
    sudo install -o root -g root -m 0755 "${src}" "${dest}" && \
        echo "✅ installed ${dest} (root:root, 0755)"
done

echo "sudoers grants: $(ls -1 /usr/local/bin/podman_socket_setup.sh | sed 's|^|/|')"
