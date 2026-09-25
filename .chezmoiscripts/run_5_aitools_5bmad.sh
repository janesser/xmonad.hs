#!/bin/bash
# run_5_aitools_5bmad.sh
#
# Publish the BMAD-METHOD skills as a PI USER-LEVEL (global) skill set under
# ~/.pi/agent/skills/ so that EVERY project — every cwd — gets the bmad skills.
#
# This REPLACES the old project-local install that wrote _bmad/ and .agents/
# inside the chezmai repo. The repo-local _bmad/ / .agents/ install is now
# OBSOLETE: the skills live at ~/.pi/agent/skills/ and are shared by all
# projects. See the BMAD section of ~/.pi/agent/AGENTS.md and the repo
# AGENTS.md, which mark the old approach as obsolete.
#
# How it works:
#   bmad's official non-interactive installer writes its pi skills to
#   <directory>/.agents/skills/  (for --tools pi). It has no flag to relocate
#   that output, so we install into a staging dir and then copy the generated
#   skills into the user-level skills directory (~/.pi/agent/skills/), which is
#   where pi looks for per-user skills that load in every cwd.
#
#   --tools pi      writes skills to .agents/skills/ (pi implements the Agent
#                   Skills spec).
#   --all-stable    resolve external modules to the highest stable tag.
#
#   NOTE on --action: bmad's interactive installer still lists `install` for a
#   first-time setup, but under `-y` (non-interactive) it rejects `install`
#   and only accepts `update` / `quick-update` (actions for an existing
#   install). So a fresh machine needs ONE interactive `npx bmad-method install`
#   to seed staging ~/.local/share/bmad; this script then keeps the skills fresh
#   with `quick-update` (re-renders files, applies minor stable upgrades,
#   preserves personas) on every cz apply.
#   --action quick-update is used unconditionally below.
#
# The bmad RUNTIME (_bmad/) is intentionally NOT installed here anymore. The
# skills reference {project-root}/_bmad/..., so to actually run a bmad session
# in a project you must install the runtime into THAT project:
#   npx bmad-method install --yes --modules bmm --tools pi \
#       --directory <project-root> --action install --all-stable
# The old repo-local _bmad/ (which this script used to keep) is obsolete.

set -uo pipefail

# Staging dir where the bmad installer can write its runtime + generated skills.
STAGE="${HOME}/.local/share/bmad"
SKILLS_DIR="${HOME}/.pi/agent/skills"

# Prerequisite: Node (the installer needs Node >= 20.12).
if ! command -v node >/dev/null 2>&1; then
  echo "[bmad] node not found — skipping BMAD update (needs Node >= 20.12)."
  exit 0
fi

mkdir -p "${STAGE}" "${SKILLS_DIR}"

# Always quick-update (see NOTE above: 'install' is invalid non-interactively).
# Quick update re-renders files, applies minor stable upgrades, preserves personas.
BMD_CMD=(npx --yes bmad-method install \
  --yes --modules bmm --tools pi \
  --directory "${STAGE}" --action quick-update --all-stable)

echo "[bmad] updating BMAD (quick-update) in staging ${STAGE} ..."

# Non-fatal: keep chezmoi apply working even if BMAD can't update right now.
"${BMD_CMD[@]}" 2>&1
rc=$?
if [ ${rc} -ne 0 ]; then
  echo "[bmad] WARNING: bmad-method install exited ${rc} (offline? stale npx?)."
  echo "[bmad] skills left as-is."
  exit 0
fi

# Publish generated skills to the pi user-level skills directory.
if cp -a "${STAGE}/.agents/skills/"* "${SKILLS_DIR}/" 2>/dev/null; then
  count=$(find "${SKILLS_DIR}" -maxdepth 1 -type d -name 'bmad-*' 2>/dev/null | wc -l | tr -d ' ')
  echo "[bmad] published ${count} bmad skills → ${SKILLS_DIR}"
  echo "[bmad] NOTE: repo-local _bmad/ install is now obsolete — skills are global."
else
  echo "[bmad] WARNING: failed to copy skills from staging to ${SKILLS_DIR}."
fi
