#!/bin/bash
# run_once_9_cleanup_llama.sh
#
# One-time teardown of the llama.cpp boot backends on a box that is NOT an AI
# box — i.e. one without a CUDA *and* without an Intel (Iris) accelerator.
# On peacewagon (Intel HD Graphics 615: no NVIDIA, non-Iris) this removes the
# llama-cuda / llama-sycl systemd units that the run_once deploy script
# (run_once_5_aitools_2llama_startup.sh) used to force-install here — they were
# failing in a CUDA-probe restart loop every 30s — plus the HuggingFace cache
# bind mount that deploy added to /etc/fstab.
#
# On AI boxes (lincopta = NVIDIA, cyberkleiber = NVIDIA + Iris) the guard below
# is false, so this script is a no-op and those backends stay put.
#
# Idempotent: every step tolerates an already-removed unit / absent fstab line,
# so a second cz apply does nothing. Sudo is used only for commands in the
# scoped NOPASSWD sudoers drop-in (systemctl, rm, sed).
#
# NOTE: `umount`/`mount` are NOT in that drop-in, so a bind mount that is still
# active at cleanup time is left mounted — it is inert once no backend serves
# its port, and only the /etc/fstab line is removed. (The repo's etc/fstab.patch
# is a different, unrelated huggingface_cache line and is left untouched.)

set -uo pipefail

# GPU-presence helpers (has_nvidia / has_intel_gpu). Sourced, not executed.
if [ -f "$HOME/.local/share/gpu.func" ] && source "$HOME/.local/share/gpu.func" 2>/dev/null; then
    if has_nvidia || has_intel_gpu; then
        echo "$(basename "$0"): AI accelerator present — leaving llama backends untouched."
        exit 0
    fi
else
    # Never guess: if we cannot positively detect the absence of an accelerator,
    # refuse to touch anything (an AI box must keep its units).
    echo "$(basename "$0"): gpu.func unavailable — refusing to act (abort, not guess)."
    exit 0
fi

echo "$(basename "$0"): no CUDA/Intel accelerator — removing llama boot backends."

UNITS=( llama-cuda.service llama-sycl.service )

# 1. Stop + disable each unit (tolerate already-gone), then remove the unit
#    files and reload systemd so the disabled units stop being restarted.
for unit in "${UNITS[@]}"; do
    sudo systemctl --system disable --now "${unit}" >/dev/null 2>&1 || true
    sudo rm -f "/etc/systemd/system/${unit}" || true
done
sudo systemctl --system daemon-reload

# 2. Drop the HuggingFace cache bind-mount line from /etc/fstab (only the llama
#    deploy added it).
if grep -q 'huggingface-hub' /etc/fstab 2>/dev/null; then
    sudo sed -i '/huggingface-hub/d' /etc/fstab
    echo "$(basename "$0"): removed the huggingface-hub bind mount from /etc/fstab."
fi

echo "$(basename "$0"): llama backends cleaned up."
exit 0
