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

* Enable .local/bin for dmenu

    ln -s ~/projs/xmonad.hs/.profile ~/.profile

### Extras

    sudo apt install \
        cargo \
        gimp \
        podman-compose \
        texworks \
        pandoc \
        dex \
        geeqie

* x2goclient
  * x2goserver-x2goagent
  * lightdm-remote-session-x2go

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
  * firefox
  * chromium

## fast terminal and console

FIXME breaks multi-user capa

    sudo apt install cargo
    cargo install alacritty # single seated, otherwise sudo
    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.cargo/bin/alacritty 50

    sudo apt install fish
    chsh
    ln -sf ~/projs/xmonad.hs/.config/fish/conf.d/ssh-env.fish ~/.config/fish/conf.d/ssh-env.fish

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

### applications alternatives

    sudo update-alternatives --config x-www-browser

### XDG default applications

Used e.g. by thunderbird

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser librewolf.desktop

### us keyboard fix

:rage: Silly time spend on this

Most startup scripts are prior to xdg-autostarts.
Keep an eye on `grep 'Exec=' /etc/xdg/autostart/*`

    sudo apt remove --purge ibus im-config # which in my case was defaulting to us-keyb

## audio setup

pipewire supercedes pulseaudio - getting bluetooth headset working

    apt install pipewire-audio easyeffects blueman
    sudo mkdir -p /etc/pipewire
    sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systenctl restart --user pipewire
    # reconnect your device

## IDE

developer tools

### Jupyter

    pipx install jupyterlab
    pipx inject jupyterlab jupyterlab-git
    jupyter-lab &
    # jupyter-lab stop

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

#### ~~VSCode~~

    snap install code
    ln -sf ~/projs/xmonad.hs/.config/Code/User/settings.json ~/.config/Code/User/settings.json
