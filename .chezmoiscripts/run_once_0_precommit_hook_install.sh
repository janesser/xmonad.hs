#!/bin/bash
# run_once_install_precommit.sh

# 1. Check if we are in a git repo (pre-commit needs .git)
if [ ! -d .git ]; then
  echo "⚠️  Not in a git repository. Skipping pre-commit installation."
  exit 0
fi

# 2. Try using uv (your preference)
if command -v uv >/dev/null 2>&1; then
  echo "🚀 uv detected. Using uvx to install pre-commit hooks..."
  # uvx runs the command in a transient environment, perfect for one-off setup
  uvx pre-commit install
  echo "✅ Pre-commit hooks installed via uvx."
  
elif command -v pip >/dev/null 2>&1; then
  echo "📦 pip detected. Installing pre-commit via pip..."
  pip install pre-commit
  pre-commit install
  echo "✅ Pre-commit hooks installed via pip."
  
else
  echo "❌ Error: Neither 'uv' nor 'pip' found. Please install pre-commit manually."
  echo "    Run: uv tool install pre-commit && pre-commit install"
  exit 1
fi
