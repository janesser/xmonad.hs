#!/bin/bash
# setup.sh

echo "🛠️  Setting up development environment..."

if command -v uv >/dev/null 2>&1; then
  echo "✨ uv detected. Using uv to install pre-commit..."
  uv tool install pre-commit
  pre-commit install
elif command -v pip >/dev/null 2>&1; then
  echo "📦 pip detected. Installing pre-commit via pip..."
  pip install pre-commit
  pre-commit install
else
  echo "❌ Error: Neither 'uv' nor 'pip' found."
  exit 1
fi

echo "✅ Setup complete! Pre-commit hooks are active."
