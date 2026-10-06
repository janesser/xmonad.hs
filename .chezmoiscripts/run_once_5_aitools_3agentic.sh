#!/bin/bash

# delete everything under ~/.pi except agent/sessions (keep conversation history)
find -mindepth 1 "$HOME/.pi" \
  -path "$HOME/.pi/agent/sessions" -prune -o \
  -path "$HOME/.pi/agent" -o \
  -exec rm -rf {} +

sudo apt install -y fd-find
sudo apt remove --purge -y fdclone
## https://github.com/earendil-works/pi/issues/3882
ln -sf /usr/bin/fdfind ~/.pi/agent/bin/fd

mise install pi
mise use -g pi

# by settings.json: pi install npm:pi-olla-autodetect
# by settings.json: pi install npm:pi-web-access

sudo snap install ghidra
# by settings.json: pi install npm:pi-ghidra
# NOTE: re-running this idempotent setup whenever the manifest
# data/pi_agent_packages.txt changes is handled by
# .chezmoiscripts/run_onchange_9_3_pi_agent_modules.sh.tmpl — editing that
# manifest changes the rendered script hash, so chezmoi re-runs this script
# on the next apply. The live ~/.pi/agent/settings.json is owned by pi.

# NB: bmad is NOT installed here anymore. The project-local install that used
# to live on this line is obsolete — the global BMAD skills are published to
# ~/.pi/agent/skills/ by run_onchange_pi_bmad.sh.tmpl. That script is gated so
# it re-runs AFTER any pi-agent upgrade/reinstall (which wipes ~/.pi), never
# before — see run_onchange_pi_bmad.sh.tmpl for the ordering rationale.

pi list
