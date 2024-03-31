# xmonad.hs

My opinionated xmonad setup.

## Essential requirements

    sudo apt install xmonad xmobar \
        light-locker-settings pcmanfm trayer xscreensaver \
        libghc-split-dev

### Recommended

* Recommended requirements

    sudo apt install shutter \
        copyq \
        x11-xserver-utils \
        network-manager-gnome \
        org-mode emacs-gtk \
        onboard \
        easyeffects

* <https://gitlab.com/bitseater/meteo>

### Extend PATH variable

For e.g. dmenu and others.

    ln -s ~/projs/xmonad.hs/.profile ~/.profile

### Extras

    sudo apt install \
        cargo \
        golang-go \
        podman-compose \
        texworks \
        pandoc \
        gimp \
        geeqie \
        dex

#### x2goclient

* x2goserver-x2goagent
* lightdm-remote-session-x2go

#### glow (shell markdown viewer)

    sudo apt install golang-go
    go install github.com/charmbracelet/glow@latest
    # PATH should expand over ~/go/bin/glow

#### Tuba (mastodon client)

## Candidates

* flatpak
* scrot
* openvoiceos
* <https://github.com/davatorium/rofi>
* <https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-TreeSelect.html>
* xcompmgr

## Dismissed

* stalonetray
* diodon
* snap
  * vscode
  * firefox, thunderbird
  * chromium
* whatsapp-for-linux

## apt sources.list.d

    sudo rsync -nrv --del etc/apt/sources.list.d/ /etc/apt/sources.list.d/
    sudo rsync -nrv --del etc/apt/keyrings/ /etc/apt/keyrings/
    # remove "n" for dry-run once assured

## fast terminal and console

FIXME breaks multi-user capa

    sudo apt install cargo
    cargo install alacritty # single seated, otherwise sudo
    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.cargo/bin/alacritty 50

    sudo apt install fish
    chsh -s $(which fish)
    ln -sf ~/projs/xmonad.hs/.config/fish/conf.d/ssh-env.fish ~/.config/fish/conf.d/ssh-env.fish
    fish_config theme choose Tomorrow
    fish_config theme save

### nala

    sudo apt install nala
    sudo nala fetch # pick wisely
    nala --install-completion=fish --show-completion=fish

## power button behaviour

Check systemd-logind

    sudo nano /etc/systemd/logind.conf

        HandlePowerKey=suspend
        IdleAction=suspend
        IdleActionSec=10min

    sudo systemctl restart systemd-logind

## default applications e.g. browser

### Librewolf

Installation instructions here: <https://librewolf.net/installation/debian/>

    update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250

Allow history storage and whitelist a few cookies.

NOTE There is no Single-Site-Browsing / Progressive-Web-App in FF-Desktop and derivatives.

#### search engines

* <https://searx.be>
* <https://ecosia.helpscoutdocs.com/article/406-about-ecosia>
* <https://stract.com/about>
* <https://about.qwant.com/en/>

### applications alternatives

    sudo update-alternatives --config x-www-browser

### XDG default applications

Used e.g. by thunderbird

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser librewolf.desktop

### mimeapps

    ln -Pfv ~/projs/xmonad.hs/.config/mimeapps.list ~/.config/mimeapps.list

### us keyboard fix

:rage: Silly time spend on this

Most startup scripts are prior to xdg-autostarts.
Keep an eye on `grep 'Exec=' /etc/xdg/autostart/*`

    sudo apt remove --purge ibus im-config # which in my case was defaulting to us-keyb

### special pipe-key combo

    ln -s ~/projs/xmonad.hs/chuwi_ubook_xpro.xkb ~/chuwi_ubook_xpro.xkb

    xkbcomp $DISPLAY chuwi_ubook_xpro.xkb
    # edit
    xkbcomp chuwi_ubook_xpro.xkb $DISPLAY

## audio setup

pipewire supercedes pulseaudio - getting bluetooth headset working

    apt install pipewire-audio easyeffects blueman
    sudo mkdir -p /etc/pipewire
    sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systenctl restart --user pipewire
    # reconnect your device

## IDE

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
