#!/bin/bash
# uv is managed by mise (replaces asdf); shims on PATH via activate.
export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)"

# TODO clean-up old python versions
## uv python list --only-installed --managed-python
uv cache clean
