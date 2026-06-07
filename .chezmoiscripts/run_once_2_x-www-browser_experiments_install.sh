#!/bin/bash

# seeking android and ubuntu capable browser (sync tabs and keepassxc compat)

# no android
if [ "$BROWSER_EXPERIMENT" = "mullvad" ]; then
    # Mullvad-Signierschlüssel herunterladen
    sudo curl -fsSLo /usr/share/keyrings/mullvad-keyring.asc https://repository.mullvad.net/deb/mullvad-keyring.asc

    # Mullvad-Repository-Server zu apt hinzufügen
    echo "deb [signed-by=/usr/share/keyrings/mullvad-keyring.asc arch=$( dpkg --print-architecture )] https://repository.mullvad.net/deb/stable stable main" | sudo tee /etc/apt/sources.list.d/mullvad.list

    # Paket installieren
    sudo apt update
    sudo apt install mullvad-browser
fi

# no keepassxc
if [ "$BROWSER_EXPERIMENT" = "ecosia" ]; then
    ## ecosia browser
    sudo apt install -y flatpak
    curl -o /tmp/org.ecosia.Browser.flatpakref https://dl.flathub.org/repo/appstream/org.ecosia.Browser.flatpakref
    # assumed to have writable space (for extensions), so in user-space of flatpak "-u"
    flatpak install -u -y /tmp/org.ecosia.Browser.flatpakref
    # flatpak uninstall --unused
fi