#!/bin/bash

export PATH=~/go/bin:~/.asdf/shims:$PATH # in case not yet set

# TODO clean-up old python versions
## uv python list --only-installed --managed-python
uv cache clean
