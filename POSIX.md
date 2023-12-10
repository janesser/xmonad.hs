
# console
## console-data
In case keymap isn't correct on xsession start.

    dpkg-reconfigure console-data

## fast terminal

    apt install cargo
    cargo install alacritty
    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.cargo/bin/alacritty 50


# power button behaviour
Check systemd-logind

    sudo nano /etc/systemd/logind.conf
    sudo systemctl restart systemd-logind

# default applications e.g. browser
## applications alternatives

    sudo update-alternatives --config x-www-browser

## XDG default applications

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser google-chrome.desktop
