# xmonad.hs

My opinionated xmonad setup. With all dependencies expanded.

## Essential requirements

    sudo apt install xmonad xmobar libghc-split-dev libghc-hostname-dev \
        light-locker-settings haskell-gtk-sni-tray-utils xscreensaver \
        pcmanfm xarchiver

Installation of xmonad xmonad-contrib can be superceded by git-based installation see COMPILE.md.

    Keep `xmonad` installed for `/usr/share/xsession/xmonad.desktop`.
    All independent `libghc*` can be removed.

### Recommended requiremends

* Recommended requirements

    sudo apt install shutter \
        copyq \
        x11-xserver-utils \
        network-manager-gnome \
        org-mode emacs-gtk \
        onboard \
        easyeffects \
        pipewire-audio \
        hw-probe \
        etckeeper \
        xpdf \
        btop

### Optional requirements

More recommended less mandatory

    sudo apt install \
        podman-compose \
        texworks \
        pandoc \
        multimedia-graphics \
        geeqie \
        dex \
        dict

### Extend PATH variable

For e.g. dmenu and others.

    ln -s ~/projs/xmonad.hs/.profile ~/.profile

### apt sources.list.d

Ubuntu Mantic 23 with some additional repositories

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
* clementine
* shotcut

### Dismissed / Deprecated

Applications now deprecated and uninstalled

* stalonetray / trayer
* diodon
* snap
  * vscode
  * firefox, thunderbird
  * chromium
* flatpak
* whatsapp-for-linux

## System utilities

    sudo apt install bumblebee-nvidia

### fast terminal and console

#### fish

    sudo apt install fish
    chsh -s $(which fish)
    ln -sf ~/projs/xmonad.hs/.config/fish/conf.d/ssh-env.fish ~/.config/fish/conf.d/ssh-env.fish
    fish_config theme choose Tomorrow
    fish_config theme save

#### zutty

<https://tomscii.sig7.se/2020/12/A-totally-biased-comparison-of-Zutty>

    sudo apt install zutty
    sudo update-alternatives --config x-terminal-emulator

#### kitty

    # latest version of kitty
    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/kovidgoyal/kitty.git --depth 1 && cd kitty
    sudo apt install libdbus-1-dev libxcursor-dev libxrandr-dev libxi-dev libxinerama-dev libgl1-mesa-dev libxkbcommon-x11-dev libfontconfig-dev libx11-xcb-dev liblcms2-dev libssl-dev libpython3-dev libxxhash-dev libsimde-dev python3-sphinx-copybutton python3-sphinx-inline-tabs libxkbcommon-x11-dev golang python3-sphinxext-opengraph furo
    # ./dev.sh build
    make linux-package
    cd linux-package
    sudo rsync -rv . /usr/local

    sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/kitty 50
    sudo update-alternatives --config x-terminal-emulator

    # integrate kitten with fish
    cd ~
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_conf.d/kitty-shell-integration.fish .config/fish/conf.d/kitty-shell-integration.fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_completions.d/clone-in-kitty.fish .config/fish/completions/clone-in-kitty.fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_completions.d/kitten.fish .config/fish/completions/kitten.fish
    ln -s /usr/local/lib/kitty/shell-integration/fish/vendor_completions.d/kitty.fish .config/fish/completions/kitty.fish

    mkdir -p ~/.config/kitty
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

    sudo cp usr/share/X11/xkb/symbols/de.chuwi_ubook_xpro /usr/share/X11/xkb/symbols/de.chuwi_ubook_xpro
    sudo cp etc/default/keyboard /etc/default/keyboard

After reboot, verify

    setxkbmap -print
        # output
        xkb_keymap {
                xkb_keycodes  { include "evdev+aliases(qwerty)" };
                xkb_types     { include "complete"      };
                xkb_compat    { include "complete"      };
                xkb_symbols   { include "pc+de.chuwi_ubook_xpro+inet(evdev)"    };
                xkb_geometry  { include "pc(pc105)"     };
        };

Good guide here:
<https://medium.com/@damko/a-simple-humble-but-comprehensive-guide-to-xkb-for-linux-6f1ad5e13450>

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

### bitseater meteo

    git clone https://github.com/SkyMaverick/statusnotifier.git
    cd statusnotifier
    diff --git a/src/meson.build b/src/meson.build
        index f457fc3..46fd5d1 100644
        --- a/src/meson.build
        +++ b/src/meson.build
        @@ -59,7 +59,7 @@ if get_option('enable_introspection')
            sni_vapi_deps = '''
                gobject-2.0
                -        gtk+-3.0
                +        gdk-pixbuf-2.0
    meson setup --reconfigure --prefix /usr/local --clearcache -D enable_introspection=true -D enable_vala=true -D enable_dbusmenu=false build/
    meson compile -C build/
    meson install -C build/

    sudo apt install build-essential git meson ninja-build valac desktop-file-utils gettext libgtk-4-dev libadwaita-1-dev libsoup-3.0-dev libjson-glib-dev libwebkitgtk-6.0-dev
    git clone --branch meteo-1.0 https://gitlab.com/janesser1/meteo.git
    cd meteo
    meson setup --prefix /usr/local build/
    cd build
    meson install

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

### eclipse

    sudo apt install snap
    sudo snap install --classic snapcraft
    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/janesser/eclipse-snap
    cd eclipse-snap
    ECLIPSE_PACKAGE=eclipse-pde
    ./try-build.sh
    snap run eclipse-pde

### kubernetes

    cd projs
    git clone https://github.com/kubernetes/kubernetes.git
    cd kubernetes
    build/run.sh make kubectl
    ln -sf $(pwd)/_output/dockerized/bin/linux/amd64/kubectl ~/.local/bin/

    kubectl version
    less ~/.kube/config
    kubectl config get-contexts

#### flux2

    nix-env -i fluxcd
    flux --version # 2.2.2
    flux install
    export GITHUB_TOKEN=<redacted>
    flux bootstrap github \
        --token-auth \
        --owner=janesser \
        --repository=fleet-infra \
        --branch=main \
        --path=clusters/qnap-k3s \
        --personal

## Social Tools

### Signal Desktop

<https://www.signal.org/download/linux/>

    sudo apt install signal-desktop

### WhatSie (Whatsapp Client)

    sudo apt install qtbase5-dev qtwebengine5-dev qtwebengine5-dev-tools
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

## Games

### Steam on Linux

    XMonad.Util.Hacks.fixSteamFlicker
    https://github.com/ValveSoftware/steam-for-linux/issues/10544

### Lutris

    apt install lutris
    # wip
