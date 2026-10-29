#!/bin/bash

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

# uv self update # only binary installer
uv tool upgrade --all
