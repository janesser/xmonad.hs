#!/bin/bash
# run_once_install_precommit.sh

pushd $CHEZMOI_SOURCE_DIR

# 1. Check if we are in a git repo (pre-commit needs .git)
if [ ! -d .git ]; then
  echo "⚠️  Not in a git repository. Skipping pre-commit installation."
  exit 1
fi

if command -v uv >/dev/null 2>&1; then
  echo "✅ uv found on PATH"
else
  eval "$(mise activate bash)"
  mise install uv
  mise use -g uv
  echo "✅ uv installed by mise"
fi

uv tool install pre-commit
pre-commit install
echo "✅ Pre-commit hooks installed via uvx."
