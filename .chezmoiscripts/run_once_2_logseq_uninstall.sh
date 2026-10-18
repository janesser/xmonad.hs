#!/bin/bash

echo Uninstalling Logseq and flatpak

flatpak uninstall -y --delete-data com.logseq.Logseq
flatpak uninstall --unused

sudo apt remove --purge -y flatpak
