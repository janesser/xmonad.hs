#!/bin/bash
# run_5_aitools_5bmad.sh
#
# Keep the project-local BMAD-METHOD harness up to date on every
# `chezmoi apply`.
#
# BMAD is installed INSIDE the chezmoz repo working dir (the repo doubles as the
# BMAD "project") via the official non-interactive installer:
#
#   npx bmad-method install --yes --modules bmm --tools pi \
#       --directory <repo> --action <install|quick-update> --all-stable ...
#
#   --tools pi      writes skills to .agents/skills/ (pi implements the Agent
#                   Skills spec; matches the current install — see the bmad
#                   installer's platform-codes.yaml).
#   --action quick-update   re-renders files, applies minor stable upgrades and
#                   preserves existing settings/personas; refuses major bumps.
#   --action install   first install (used when no _bmad/ exists yet).
#   --all-stable     resolve external modules to the highest stable tag.
#
# Generated dirs (_bmad/, .agents/, _bmad-output/) are git-/chezmoi-ignored
# artifacts this script regenerates — they are never tracked in source state.
#
# Resilience: a failure (offline / npm unreachable / version conflict) must NOT
# break `chezmoi apply`, so a non-zero installer exit is logged and ignored.

set -uo pipefail

SRC_DIR="${CHEZMOI_SOURCE_DIR:-.}"
REPO="${SRC_DIR}"

# Sanity: only act when we're actually inside the chezmoz repo.
if [ ! -f "${REPO}/.chezmoiignore" ]; then
  echo "[bmad] not inside a chezmoi repo (${REPO}) — skipping BMAD update."
  exit 0
fi

# Prerequisite: Node (the installer needs Node >= 20.12).
if ! command -v node >/dev/null 2>&1; then
  echo "[bmad] node not found — skipping BMAD update (needs Node >= 20.12)."
  exit 0
fi

# Fresh install vs quick-update (existing _bmad/).
if [ -d "${REPO}/_bmad" ]; then
  BMAD_ACTION=quick-update
  # Quick update only touches what's needed and preserves existing settings.
  BMD_CMD=(npx --yes bmad-method install \
    --yes --modules bmm --tools pi \
    --directory "${REPO}" --action quick-update --all-stable)
else
  BMAD_ACTION=install
  # First install: seed the personal defaults that live in the git-ignored
  # _bmad/config.user.toml on this machine.
  BMD_CMD=(npx --yes bmad-method install \
    --yes --modules bmm --tools pi \
    --directory "${REPO}" --action install --all-stable \
    --user-name Jan \
    --communication-language English \
    --document-output-language English)
fi

echo "[bmad] updating BMAD (action=${BMAD_ACTION}) in ${REPO} ..."

# Non-fatal: keep chezmoi apply working even if BMAD can't update right now.
if "${BMD_CMD[@]}" 2>&1; then
  echo "[bmad] BMAD update complete."
else
  rc=$?
  echo "[bmad] WARNING: bmad-method install exited ${rc} (offline? stale npx?)."
  echo "[bmad] BMAD left as-is; restore network and run `cz apply` again."
fi
