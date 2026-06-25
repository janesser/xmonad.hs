#!/bin/bash

exit 0 # skip for now

uv venv --allow-existing /home/jan/.unsloth/studio/unsloth_studio
source .unsloth/studio/unsloth_studio/bin/activate
uv pip install unsloth
uv pip install cryptography structlog fastapi uvicorn matplotlib python-multipart diceware pyjwt
unsloth studio update
# unsloth studio