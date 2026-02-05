#!/bin/bash

# ollama
## https://docs.ollama.com/linux
## https://github.com/jameschrisa/Ollama_Tuning_Guide/blob/main/docs/cpu-optimization.md

## https://github.com/ollama/ollama/issues/3976
if command -v ollama >/dev/null
then
    echo ollama already installed.
else
    curl -fsSL https://ollama.com/install.sh | sh
fi
