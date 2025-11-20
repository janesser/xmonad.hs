#!/bin/bash

if [ "$CHEZMOI_ARCH" = "amd64" ]; then
    sudo extrepo enable librewolf
    sudo apt update
    sudo apt install -y librewolf
    sudo update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250
    XDG_DEFAULT=librewolf.desktop
elif [ "$CHEZMOI_ARCH" = "arm64" ]; then
    cd "$CHEZMOI_SOURCE_DIR"
    sudo cp etc/cron.daily/vivaldi /etc/cron.daily/
    sudo cp etc/default/vivaldi /etc/default/
    sudo repo_add_once=true /etc/cron.daily/vivaldi
    sudo apt update
    sudo apt install -y vivaldi-stable
    XDG_DEFAULT=vivaldi-stable.desktop
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
