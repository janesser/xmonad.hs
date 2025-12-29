#!/bin/bash

if [ "$CHEZMOI_ARCH" = "amd64" ]; then
    sudo extrepo enable librewolf
    sudo apt update
    sudo apt install -y librewolf
    sudo update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250
    XDG_DEFAULT=librewolf.desktop

    curl -o /tmp/keepassxc_browser.xpi https://addons.mozilla.org/firefox/downloads/file/4628286/keepassxc_browser-1.9.11.xpi
    librewolf /tmp/keepassxc_browser.xpi &
elif [ "$CHEZMOI_ARCH" = "arm64" ]; then
    ~/pi-apps/manage install Librewolf
    XDG_DEFAULT=librewolf.desktop

    ~/pi-apps/manage uninstall Chromium
    ~/pi-apps/manage uninstall "Better Chromium"
else
    echo $CHEZMOI_ARCH no default browser defined.
    exit 1
fi

sudo update-alternatives --auto x-www-browser

if command -v xdg-settings >/dev/null
then
    xdg-settings set default-web-browser "$XDG_DEFAULT"
else
    echo xdg defaults were NOT set.
fi
