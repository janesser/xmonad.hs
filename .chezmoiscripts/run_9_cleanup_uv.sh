#!/bin/bash

# TODO clean-up old python versions
## uv python list --only-installed --managed-python
uv cache clean

rm -f ~/.local/bin/uv ~/.local/bin/uvx # remove installer binaries, which are no asdf managed