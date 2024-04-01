# xmonad.hs

My opinionated xmonad setup. With all dependencies expanded.

## Essential requirements

    sudo apt install xmonad xmobar \
        light-locker-settings pcmanfm trayer xscreensaver \
        libghc-split-dev

### Recommended requiremends

* Recommended requirements

    sudo apt install shutter \
        copyq \
        x11-xserver-utils \
        network-manager-gnome \
        org-mode emacs-gtk \
        onboard \
        easyeffects

* <https://gitlab.com/bitseater/meteo>

### Optional requirements

More recommended less mandatory

    sudo apt install \
        podman-compose \
        texworks \
        pandoc \
        gimp \
        geeqie \
        dex

### Extend PATH variable

For e.g. dmenu and others.

    ln -s ~/projs/xmonad.hs/.profile ~/.profile

### apt sources.list.d

    sudo rsync -nrv --del etc/apt/sources.list.d/ /etc/apt/sources.list.d/
    sudo rsync -nrv --del etc/apt/keyrings/ /etc/apt/keyrings/
    # remove "n" for dry-run once assured

### power button behaviour

Check systemd-logind

    sudo nano /etc/systemd/logind.conf

        HandlePowerKey=suspend
        IdleAction=suspend
        IdleActionSec=10min

    sudo systemctl restart systemd-logind

## Candidates & Experiments

* scrot
* openvoiceos
* <https://github.com/davatorium/rofi>
* <https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-TreeSelect.html>
* xcompmgr

### Dismissed / Deprecated

Applications now deprecated and uninstalled

* stalonetray
* diodon
* snap
  * vscode
  * firefox, thunderbird
  * chromium
* flatpak
* whatsapp-for-linux

## System utilities

### fast terminal and console

    sudo apt install fish
    chsh -s $(which fish)
    ln -sf ~/projs/xmonad.hs/.config/fish/conf.d/ssh-env.fish ~/.config/fish/conf.d/ssh-env.fish
    fish_config theme choose Tomorrow
    fish_config theme save


    # latest version of kitty
    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/kovidgoyal/kitty.git --depth 1 && cd kitty
    sudo apt install libdbus-1-dev libxcursor-dev libxrandr-dev libxi-dev libxinerama-dev libgl1-mesa-dev libxkbcommon-x11-dev libfontconfig-dev libx11-xcb-dev liblcms2-dev libssl-dev libpython3-dev libxxhash-dev libsimde-dev python3-sphinx-copybutton python3-sphinx-inline-tabs
    # ./dev.sh build
    make linux-package
    cd linux-package
    sudo rsync -rv . /usr/local

    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/kitty 50
    sudo update-alternatives --config x-terminal-emulator

    # integrate kitten with fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_conf.d/kitty-shell-integration.fish .config/fish/conf.d/kitty-shell-integration.fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_completions.d/clone-in-kitty.fish .config/fish/completions/clone-in-kitty.fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_completions.d/kitten.fish .config/fish/completions/kitten.fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_completions.d/kitty.fish .config/fish/completions/kitty.fish

    mkdir ~/.config/kitty
    cp ~/projs/xmonad.hs/.config/kitty/* ~/.config/kitty/

### nala

    sudo apt install nala
    sudo nala fetch # pick wisely
    nala --install-completion=fish --show-completion=fish

### mimeapps

    ln -Pfv ~/projs/xmonad.hs/.config/mimeapps.list ~/.config/mimeapps.list

### us keyboard fix

:rage: Silly time spend on this

Most startup scripts are prior to xdg-autostarts.
Keep an eye on `grep 'Exec=' /etc/xdg/autostart/*`

    sudo apt remove --purge ibus im-config # which in my case was defaulting to us-keyb

### special pipe-key combo (tablet keyboard)

    ln -s ~/projs/xmonad.hs/chuwi_ubook_xpro.xkb ~/chuwi_ubook_xpro.xkb

    xkbcomp $DISPLAY chuwi_ubook_xpro.xkb
    # edit
    xkbcomp chuwi_ubook_xpro.xkb $DISPLAY

### audio setup

pipewire supercedes pulseaudio - getting bluetooth headset working

    sudo apt install pipewire-audio easyeffects blueman
    sudo mkdir -p /etc/pipewire
    sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systenctl restart --user pipewire
    # reconnect your device

### x2goclient

* x2goserver-x2goagent
* lightdm-remote-session-x2go

## Office applications

### Librewolf

Installation instructions here: <https://librewolf.net/installation/debian/>

    sudo update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250

Allow history storage and whitelist a few cookies.

NOTE There is no Single-Site-Browsing / Progressive-Web-App in FF-Desktop and derivatives.

#### search engines

* <https://searx.be>
* <https://ecosia.helpscoutdocs.com/article/406-about-ecosia>
* <https://stract.com/about>
* <https://about.qwant.com/en/>

#### XDG default applications

Used e.g. by thunderbird

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser librewolf.desktop

## Developer Tools

developer tools

### GHCup

Used for haskell-language-server. <https://www.haskell.org/ghcup/>

    sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
    # answer Y to haskell-language-server
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

### Codium

VSCode without telemetry & tracking, see: <https://vscodium.com/#why>

Installation instructions here: <https://www.linuxcapable.com/install-vscodium-on-ubuntu-linux/>

    ln -sf ~/projs/xmonad.hs/.config/VSCodium/User/settings.json ~/.config/VSCodium/User/settings.json

#### Extensions

    codium --list-extensions
        bmalehorn.vscode-fish
        DavidAnson.vscode-markdownlint
        foxundermoon.shell-format
        haskell.haskell
        justusadam.language-haskell
        mads-hartmann.bash-ide-vscode
    codium --install-extension (<extension-id> | <extension-vsix-path>)
    ln -sf ~/projs/xmonad.hs/.config/VSCodium/User/settings.json ~/.config/VSCodium/User/settings.json

### Jupyter

    pipx install jupyterlab
    pipx inject jupyterlab jupyterlab-git
    jupyter-lab &
    # jupyter-lab stop

### glow (shell markdown viewer)

    sudo apt install golang-go
    go install github.com/charmbracelet/glow@latest
    # PATH should expand over ~/go/bin/glow

## Social Tools

### Signal Desktop

<https://www.signal.org/download/linux/>

    sudo apt install signal-desktop

### WhatSie (Whatsapp Client)

    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/keshavbhatt/whatsie.git
    cd whatsie/src
    qmake PREFIX=/usr/local
    make -j4
    sudo make install

### Tuba (Mastodon Client)

    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/GeopJr/Tuba.git
    cd Tuba
    git checkout v0.6.3
    sudo apt install meson valac libjson-glib-dev libxml2-dev libgee-0.8-dev libsoup-3.0-dev libadwaita-1-dev libsecret-1-dev libgtksourceview-5-dev
    meson setup --prefix /usr/local --reconfigure builddir/
    sudo make install

### Element Desktop (Matrix.org Client)

<https://element.io/download>

### Mail Client

    # alternatively
    sudo apt install evolution
    # or
    sudo apt install claws-mail claws-mail-vcalendar-plugin

    sudo update-alternatives --install /usr/bin/x-mail-client x-mail-client /usr/bin/evolution 50
    sudo update-alternatives --install /usr/bin/x-mail-client x-mail-client /usr/bin/claws-mail 50
