#!/bin/bash
# npm comes from the mise-managed node; put mise's shims on PATH.
export PATH="$HOME/.local/bin:$PATH"
eval "$(mise activate bash)"
npm cache verify
