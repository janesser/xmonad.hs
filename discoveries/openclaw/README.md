# openclaw

Formerly moltbot, formerly clawdbot

<https://www.bitdoze.com/clawdbot-setup-guide/>

## Install openclaw

    nvm use lts
    npm i -g openclaw # 2026.1.29
    openclaw setup

## Install openclaw gateway

    openclaw config set gateway.mode local
    openclaw gateway install --force
    openclaw gateway restart
    openclaw gateway status

## Configure Ollama model provider

    ollama pull mistral-small3.2:latest
    openclaw models auth paste-token --provider ollama # enter anything
    openclaw models list --all --local
    openclaw models set mistral-small3.2:latest
    openclaw logs --follow

This model is too big for my local setup.

## Install LM Studio

<https://docs.openclaw.ai/concepts/model-providers#local-proxies-lm-studio,-vllm,-litellm,-etc>

    chmod +x ~/Downloads/LM-Studio-0.4.0-18-x64.AppImage
    sudo mv ~/Downloads/LM-Studio-0.4.0-18-x64.AppImage /usr/local/bin/
    sudo nano ~/.local/bin/lmstudio.sh

        #!/bin/sh
        ~/Downloads/LM-Studio-0.4.0-18-x64.AppImage --no-sandbox # https://github.com/lmstudio-ai/lmstudio-bug-tracker/issues/624

