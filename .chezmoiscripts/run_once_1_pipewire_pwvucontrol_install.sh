#!/bin/bash

if [[ "$(hostname)" = "lincopta" || "$(hostname)" = "flowerpower" ]]; then
    echo Installing pwvucontrol.
else
    echo Not supported on this machine.
    exit 0
fi

if command -v pwvucontrol; then
    echo you have a pwvucontrol, reinstalling to potentially have the same/newer version.
fi

if command -v yay; then
    yay -S --noconfirm mpv
elif command -v apt; then
    sudo apt install -y meson \
        libgtk-4-dev libadwaita-1-dev libpipewire-0.3-dev libwireplumber-0.5-dev \
        blueprint-compiler gettext clang
    mkdir -p ~/projs/; cd ~/projs
    git clone https://github.com/saivert/pwvucontrol.git
    cd pwvucontrol
    meson setup builddir
    meson compile -C builddir
    meson install -C builddir # TODO uninstaller missing
fi
