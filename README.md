
# xmonad.hs

My opinionated xmonad setup.

## Requirements

    apt-get install xmonad xmobar libpango1.0-dev shutter copyq x11-xserver-utils network-manager-gnome light-locker-settings pcmanfm trayer
    
### Extras

    cargo

## Candidates

    dex kupfer flatpak podman scrot i3blocks

    openvoiceos

    https://github.com/davatorium/rofi
    https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Prompt/OrgMode.hs

## Dismissed

    stalonetray
    diodon

# console

## fast terminal
FIXME breaks multi-user capa

    sudo apt install cargo
    cargo install alacritty
    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.cargo/bin/alacritty 50

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
pipewire supercedes pulseaudio - finally ?

    apt install pipewire-audio easyeffects
    sudo mkdir -p /etc/pipewire
    sudo cp /usr/share/pipewire/pipewire.conf /etc/pipewire/pipewire.conf
    systenctl restart --user pipewire
    # reconnect your device