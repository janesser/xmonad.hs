#!/bin/bash

source /etc/bash.bashrc

alias cz=chezmoi

setenv () {
  export $1=$2
}

if [ -f "~/.ssh/env" ]; then
  source ~/.ssh/env
fi
