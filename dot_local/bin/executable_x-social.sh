#!/bin/bash

source .local/share/start_once.func

start_once x-mail-client
start_once signal-desktop
start_once signal-desktop-unofficial # arm64 packaging
start_once zapzap
# start_once element-desktop
# INSUFFICIENT_USE start_once dev.geopjr.Tuba # compiled from github
