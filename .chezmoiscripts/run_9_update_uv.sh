#!/bin/bash

export PATH=~/go/bin:~/.asdf/shims:$PATH # in case not yet set

# uv self update # only binary installer
uv tool upgrade --all
