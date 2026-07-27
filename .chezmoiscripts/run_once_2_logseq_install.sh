#!/bin/bash

sudo apt install -y flatpak

flatpak remote-add -u --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

flatpak install -u -y flathub com.logseq.Logseq