#!/bin/bash
# Install/update the agent skill bundle (chezmoi-skill) for Pi Agent and OpenCode.
# Sourced from this repo's git remote — `git remote -v` shows it:
#   origin  git@github.com:janesser/chezmoi-skill.git
set -euo pipefail

REPO_URL="git@github.com:janesser/chezmoi-skill.git"
SRC_DIR="/home/jan/projs/chezmoi-skill/.agents/skills"
AGENT_DIR="$HOME/.agents/skills"
CODEX_DIR="$HOME/.codex/skills"

# Bring the source up to date (skip if offline; skills still load from what's cached).
if command -v git >/dev/null 2>&1 && [ -d "/home/jan/projs/chezmoi-skill/.git" ]; then
  cd /home/jan/projs/chezmoi-skill || exit 1
  git fetch origin 2>/dev/null || true
fi

for skill in $(ls "$SRC_DIR"); do
  # Skip anything that isn't a skill directory.
  [ -f "$SRC_DIR/$skill/SKILL.md" ] || continue

  mkdir -p "$AGENT_DIR/$skill"
  cp "$SRC_DIR/$skill/SKILL.md" "$AGENT_DIR/$skill/SKILL.md"

  # OpenCode discovers skills through symlinks back into ~/.agents/skills/.
  ln -sfn "../../.agents/skills/$skill" "$CODEX_DIR/$skill"
done
