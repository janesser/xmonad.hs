#!/bin/bash

sudo apt install -y pipx
pipx install -f uv
uv venv --allow-existing
uv pip install -U poetry