#!/bin/bash
# Install/update the agent skills (chezmoi-skill) for Pi Agent and OpenCode.
# Skills are always installed from the upstream repo, never from a local path:
#   origin  git@github.com:janesser/chezmoi-skill.git
set -euo pipefail

SKILL_NAME=chezmoi

# All nine skills of the bundle, each into its own directory in ~/.agents/skills/.
# -a pi opencode : install only for Pi Agent and OpenCode (both read ~/.agents/skills/)
# --copy         : copy files instead of symlinking them into other agents' directories
# -s             : select a single skill by name
npx --yes skills add git@github.com:janesser/chezmoi-skill.git \
  -g -a pi opencode --copy -y \
  -s "${SKILL_NAME}-add" \
  -s "${SKILL_NAME}-apply" \
  -s "${SKILL_NAME}-init" \
  -s "${SKILL_NAME}-install" \
  -s "${SKILL_NAME}-refactor" \
  -s "${SKILL_NAME}-review" \
  -s "${SKILL_NAME}-status" \
  -s "${SKILL_NAME}-uninstall" \
  -s "${SKILL_NAME}-update"
