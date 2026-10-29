#!/bin/bash

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

# TODO clean-up old python versions
## uv python list --only-installed --managed-python
uv cache clean
