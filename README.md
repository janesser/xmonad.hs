
# xmonad.hs

My opinionated xmonad setup.

## Requirements

    apt-get install xmonad xmobar shutter copyq x11-xserver-utils network-manager-gnome light-locker-settings pcmanfm trayer dex
    
### Extras

* cargo
* gimp
* org-mode emacs-gtk
* x2goclient
* podman-compose
* snap

## Candidates

* flatpak
* scrot
* openvoiceos
* https://github.com/davatorium/rofi

## Dismissed
- stalonetray
- diodon

# console

## fast terminal
FIXME breaks multi-user capa

    sudo apt install cargo
    cargo install alacritty # single seated, otherwise sudo
    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.cargo/bin/alacritty 50

    sudo apt install fish
    chsh

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