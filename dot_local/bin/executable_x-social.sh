#!/bin/bash

# start_once lives in one central place. Use an absolute path so it loads
# regardless of the caller's cwd (xmonad spawns this via an include).
source "$HOME/.local/share/start_once.func"

start_once x-mail-client
start_once signal-desktop
start_once zapzap
