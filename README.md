
# xmonad.hs

My opinionated xmonad setup.

## Requirements

    apt-get install xmonad xmobar libpango1.0-dev kupfer shutter diodon x11-xserver-utils trayer network-manager-gnome light-locker-settings

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

    sudo systemctl restart systemd-logind

# default applications e.g. browser

## applications alternatives

    sudo update-alternatives --config x-www-browser

## XDG default applications
Used e.g. by thunderbird

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser google-chrome.desktop
