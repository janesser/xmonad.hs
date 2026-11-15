#!/bin/bash

# only on apt machine
command -v apt && sudo apt install -y fd-find

command -v snap && sudo snap install ghidra


source "$CHEZMOI_SOURCE_DIR/commons/mise_assert-pi.sh"
# pi extensions install by settings.json
pi list


# https://docs.bmad-method.org/start/install-bmad/
## npx --yes bmad-method install --help

npx --yes bmad-method install --yes --tools pi,opencode
