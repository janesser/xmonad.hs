#!/bin/bash
# run_once_5_aitools_8opencode.sh
#
# United setup for jan's opencode instances — one userspace unit, shared by
# every box that has opencode on PATH:
#
#   ~/.config/systemd/user/opencode.service -> the united userspace unit
#                                             (from dot_config/systemd/user/)
#   opencode on PATH                       -> the binary to run; it lands there
#                                             either way:
#       /home/jan/.local/share/mise/installs/node/latest/bin/opencode
#                                            npm/mise install (the AI server)
#       /home/jan/.local/share/mise/shims/opencode
#                                            mise shim -> /usr/bin/mise (flowerpower)
#       /home/jan/.local/bin/opencode
#                                            standalone arm64 (raspberrypi)
#
# The web UI runs under the united userspace unit on every interface at port
# 49374:
#
#   http://<box>:49374          # browser web UI (via 'opencode pair')
#   opencode --server :49374    # TUI connected to the same server
#
# The provider config is rendered by chezmoi from
# dot_config/opencode/opencode.json onto this box:
#
#   llama-cuda-card1/sage-a1.5     -> :8081 (V100 SXM2 CUDA0, llama-cuda.service)
#   llama-cuda-card2/sage-a1.5     -> :8082 (PCIe V100 card 2, llama-little-brother.service)
#
# Both cards serve InternScience/Sage-A1.5-Q4_K_M-GGUF:Q4_K_M via the OpenAI
# compatible API at /v1/models and /v1/chat/completions. The provider key (left
# of `/`) is an opencode-side label; the model ID (right of `/`) is sent to the
# server, which serves the loaded model regardless of the name it receives.
#
# This script runs on every box: opencode must be on PATH — install it if not.
# Then enable + start the userspace unit and verify the backends opencode will
# talk to. On any host where opencode cannot be installed, this is a no-op
# (the unit stays disabled). The obsolete system units that used to run here are
# disabled by this script during the transition (their sources are gone from the repo).
#
# Sudo is used only for commands in the scoped NOPASSWD sudoers drop-in (see
# AGENTS.md): install, systemctl. `npm i -g` writes into the mise node dir and
# needs no sudo. The unit is a chezmoi target under dot_config/systemd/user/ —
# it renders into ~/.config/systemd/user/ and only needs to be enabled here.
#
# Subsumes .chezmoiscripts/run_once_5_aitools_8b_opencode_pi.sh (raspberrypi):
# the unit is shared, so there is no separate Pi install script anymore.

set -euo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
UNIT=opencode.service
UNIT_SRC="${SRC_DIR}/dot_config/systemd/user/${UNIT}"
UNIT_DEST="${HOME}/.config/systemd/user/${UNIT}"
UI_PORT=49374

mise use opencode@latest

# cleanup
for unit in opencode-web.service opencode-pi.service; do
    if [ -f "/etc/systemd/system/${unit}" ]; then
        if sudo systemctl disable --now "${unit}" >/dev/null 2>&1; then
            echo "✅ disabled + stopped /etc/systemd/system/${unit} (replaced by ${UNIT})"
        else
            echo "⚠️  could not disable /etc/systemd/system/${unit} — remove it by hand if it stays enabled"
        fi
    else
        echo "=> /etc/systemd/system/${unit} is not installed (nothing to do)"
    fi
done
