#!/bin/bash
#
# Re-run `pi update --extensions` on every `cz apply`.
#
# This restores the old `run_9_update_pi.sh` (which ran `pi update --all` and
# was deleted in ab0d52a "pi by mise update") with a narrower scope: only the
# pi-agent extensions listed in `data/pi_agent_packages.txt` (installed via
# `pi install npm:pi-...` into `~/.pi/agent/settings.json`).
#
# Safeguards:
#   * `pi` must be on PATH (mise-installed, `~/.local/bin/pi`) — otherwise skip
#   * `pi update --extensions` needs a live SSH agent for git access; a dead
#     agent makes git fetch fail, so the command is non-fatal and `cz apply`
#     never breaks
#   * `--no-approve` is used so the command does not trust project-local files
#     (it runs in the chezmoi source dir otherwise)
#
# Non-fatal: always exits 0 so an unavailable `pi` or a failed update never
# breaks `cz apply`.

set -uo pipefail

export PATH="$HOME/.local/bin:$PATH"

if ! command -v pi >/dev/null 2>&1; then
    echo "==> pi: 'pi' not on PATH (mise not installed?); skipping 'pi update --extensions'."
    exit 0
fi

echo "==> pi: running 'pi update --extensions'..."
pi update --extensions --no-approve || {
    echo "==> pi: 'pi update --extensions' failed (non-fatal)."
    exit 0
}

echo "==> pi: 'pi update --extensions' finished."
exit 0
