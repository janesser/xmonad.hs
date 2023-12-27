
# xmonad.hs

My opinionated xmonad setup.

## Requirements

    apt-get install xmonad xmobar shutter copyq x11-xserver-utils network-manager-gnome light-locker-settings pcmanfm trayer dex

### Extras

* cargo
* gimp
* org-mode emacs-gtk
* x2goclient
  * x2goserver-x2goagent
  * lightdm-remote-session-x2go
* podman-compose
* snap
* xcompmgr

## Candidates

* flatpak
* scrot
* openvoiceos
* <https://github.com/davatorium/rofi>

## Dismissed

* stalonetray
* diodon

# console

## fast terminal

FIXME breaks multi-user capa

    sudo apt install cargo
    cargo install alacritty # single seated, otherwise sudo
    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.cargo/bin/alacritty 50

    sudo apt install fish
    chsh
    ln -sf ~/projs/xmonad.hs/.config/fish/conf.d/ssh-env.fish ~/.config/fish/conf.d/ssh-env.fish

# power button behaviour

Check systemd-logind

    sudo nano /etc/systemd/logind.conf

        HandlePowerKey=suspend
        IdleAction=suspend
        IdleActionSec=10min

    sudo systemctl restart systemd-logind

# default applications e.g. browser

## applications alternatives

    sudo update-alternatives --config x-www-browser

## XDG default applications

Used e.g. by thunderbird

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser google-chrome.desktop

# audio setup

pipewire supercedes pulseaudio - getting bluetooth headset working

    apt install pipewire-audio easyeffects
    sudo mkdir -p /etc/pipewire
    sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systenctl restart --user pipewire
    # reconnect your device

# Jupyter

    pipx install jupyterlab
    pipx inject jupyterlab jupyterlab-git
    jupyter-lab &
    # jupyter-lab stop

# GHCup

Used for haskell-language-server. <https://www.haskell.org/ghcup/>

    sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
    # answer Y to haskell-language-server
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# VSCOde

    snap install code
    ln -sf ~/projs/xmonad.hs/.config/Code/User/settings.json ~/.config/Code/User/settings.json
