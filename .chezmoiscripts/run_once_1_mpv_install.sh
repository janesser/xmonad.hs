#!/bin/bash

if command -v mpv; then
    echo you have a mpv, reinstalling to potentially have the same/newer version.
fi

if command -v yay; then
    yay -S --noconfirm mpv
elif command -v apt; then
    sudo apt install -y flatpak
    flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
    flatpak install flathub io.mpv.Mpv
fi