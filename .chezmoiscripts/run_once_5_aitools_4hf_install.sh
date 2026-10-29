#!/bin/bash

source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-uv.sh"

uv tool install hf
hf skills add -g
