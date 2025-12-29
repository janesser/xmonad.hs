#!/bin/bash

if command -v ~/pi-apps/manage >/dev/null &2>1
then
    ~/pi-apps/manage updater
fi