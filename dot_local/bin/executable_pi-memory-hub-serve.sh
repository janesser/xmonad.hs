#!/usr/bin/env bash
# pi-memory-hub-serve.sh — launcher for the pi-memory-hub systemd unit.
#
# Resolves node (mise shims first, then the system path) and execs the
# hub-src checkout's local tsx against the hub server entry. The unit is
# Type=exec, so this exec'd node process IS the unit's main process (the same
# pattern as the llama launchers).
set -euo pipefail

HUB_SRC="${HOME}/.local/share/pi-hermes-memory/hub-src"

# --- resolve node -------------------------------------------------------------
NODE=""
for cand in \
    "${HOME}/.local/share/mise/shims/node" \
    /usr/bin/node; do
    if [ -x "${cand}" ] && "${cand}" --version >/dev/null 2>&1; then
        NODE="${cand}"
        break
    fi
done
if [ -z "${NODE}" ]; then
    echo "pi-memory-hub: no usable node found (checked mise shims and /usr/bin)" >&2
    exit 1
fi

# --- sanity: checkout + tsx present -------------------------------------------
if [ ! -f "${HUB_SRC}/src/hub/server.ts" ]; then
    echo "pi-memory-hub: checkout missing at ${HUB_SRC} — run the chezmoi run script first" >&2
    exit 1
fi
if [ ! -x "${HUB_SRC}/node_modules/.bin/tsx" ]; then
    echo "pi-memory-hub: tsx missing in ${HUB_SRC} — run 'npm ci' there first" >&2
    exit 1
fi

exec "${NODE}" "${HUB_SRC}/node_modules/.bin/tsx" "${HUB_SRC}/src/hub/server.ts"
