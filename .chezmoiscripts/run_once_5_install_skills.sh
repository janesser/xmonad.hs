#!/bin/bash
# Install/update the agent skill bundle (chezmoi-skill) for Pi Agent and OpenCode.
# Skills are always installed from the upstream repo, never from a local path:
#   origin  git@github.com:janesser/chezmoi-skill.git
set -euo pipefail

REPO_URL="git@github.com:janesser/chezmoi-skill.git"
CLONE_DIR="$HOME/projs/chezmoi-skill"
SRC_DIR="$CLONE_DIR/.agents/skills"
AGENT_DIR="$HOME/.agents/skills"
CODEX_DIR="$HOME/.codex/skills"

# Bring the upstream source into place: clone on first run, fetch on later ones.
if command -v git >/dev/null 2>&1; then
  if [ -d "$CLONE_DIR/.git" ]; then
    ( cd "$CLONE_DIR" && git fetch origin 2>/dev/null || true )
  else
    ( mkdir -p "$HOME/projs" && git clone --quiet "$REPO_URL" "$CLONE_DIR" 2>/dev/null || true )
  fi
fi

# No source on this machine (offline / no git) — skills keep loading from what's cached.
[ -d "$SRC_DIR" ] || exit 0

mkdir -p "$CODEX_DIR"

for skill in $(ls "$SRC_DIR"); do
  # Skip anything that isn't a skill directory.
  [ -f "$SRC_DIR/$skill/SKILL.md" ] || continue

  mkdir -p "$AGENT_DIR/$skill"
  cp "$SRC_DIR/$skill/SKILL.md" "$AGENT_DIR/$skill/SKILL.md"

  # OpenCode discovers skills through symlinks back into ~/.agents/skills/.
  ln -sfn "../../.agents/skills/$skill" "$CODEX_DIR/$skill"
done
