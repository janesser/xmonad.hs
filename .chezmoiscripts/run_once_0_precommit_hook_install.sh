#!/bin/bash
# run_once_install_precommit.sh

pushd $CHEZMOI_SOURCE_DIR

# 1. Check if we are in a git repo (pre-commit needs .git)
if [ ! -d .git ]; then
  echo "⚠️  Not in a git repository. Skipping pre-commit installation."
  exit 1
fi

# 2. Try using uv (your preference)
if command -v uv >/dev/null 2>&1; then
  echo "🚀 uv detected. Using uvx to install pre-commit hooks..."
  # uvx runs the command in a transient environment, perfect for one-off setup
  uv tool install pre-commit
  pre-commit install
  echo "✅ Pre-commit hooks installed via uvx."
  
else
  echo ⚠️  No uvx available
  exit 1
fi
