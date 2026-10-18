#!/bin/bash
# run_once_install_precommit.sh

pushd $CHEZMOI_SOURCE_DIR

# 1. Check if we are in a git repo (pre-commit needs .git)
if [ ! -d .git ]; then
  echo "⚠️  Not in a git repository. Skipping pre-commit installation."
  exit 1
fi

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

uv tool install pre-commit
pre-commit install
echo "✅ Pre-commit hooks installed via uvx."
