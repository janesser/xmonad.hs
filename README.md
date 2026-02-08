# xmonad.hs

My opinionated xmonad & chezmoi setup. With all dependencies expanded.

## Next steps

<https://github.com/imthenachoman/How-To-Secure-A-Linux-Server>

* libpam-google-authenticator
* hardening ssh...

## Dotfiles via chezmoi

### via snap

    sudo snap install chezmoi --classic
    ssh-keygen # add pubkey to github settings
    sudo apt install git
    chezmoi init git@github.com:janesser/xmonad.hs.git
    chezmoi age-keygen -o ~/.config/chezmoi/age-id.txt
    chezmoi apply -k

### manual install

    sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin # https://www.chezmoi.io/install/ (snap or pre-compiled deb package)
    ssh-keygen # add pubkey to github settings
    sudo apt install git
    ~/.local/bin/chezmoi init git@github.com:janesser/xmonad.hs.git
    ~/.local/bin/chezmoi age-keygen -o ~/.config/chezmoi/age-id.txt
    ~/.local/bin/chezmoi apply -k

## Awesome packages (ubuntu)

`run_once_0_essentials_install.sh.tmpl`

### essential

    sudo apt install xmonad xmobar libghc-split-dev libghc-hostname-dev \
        haskell-gtk-sni-tray-utils xscreensaver \
        pcmanfm xarchiver

Installation of xmonad xmonad-contrib can be superceded by git-based installation see COMPILE.md.

    Keep `xmonad` installed for `/usr/share/xsession/xmonad.desktop`.
    All independent `libghc*` can be removed.

### recommended

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
        btop rocm-smi \
        clamtk \
        system-config-printer

About btop migrating from rocm-smi to amd-smi: <https://github.com/aristocratos/btop/issues/1196>

### extras

More recommended less mandatory

    sudo apt install \
        podman-compose \
        texworks texlive-humanities \
        pandoc \
        multimedia-graphics \
        geeqie \
        dex \
        dict \
        apt-listchanges

### apt sources.list.d

Ubuntu Noble 24 with some additional repositories

    sudo rsync -nrv --del etc/apt/sources.list.d/ /etc/apt/sources.list.d/
    sudo rsync -nrv --del etc/apt/keyrings/ /etc/apt/keyrings/
    # remove "n" for dry-run once assured

`run_once_0_apt_sources.sh`

### ssh_config

resolve hostnames differently for localnetwork

<https://fmartingr.com/blog/2022/08/12/using-ssh-config-match-to-connect-to-a-host-using-multiple-ip-or-hostnames/>

#### x11forwarding

    ssh -X ...
    export XAUTHORITY=$HOME/.Xauthority
    ...

#### mobile ssh - mosh

<https://linuxhandbook.com/mosh/>

### power button behaviour

Check systemd-logind

    sudo nano /etc/systemd/logind.conf

        HandlePowerKey=suspend
        IdleAction=suspend

    sudo systemctl restart systemd-logind

Check idleHint

    loginctl show-seat -p IdleHint
    # probably "idleHint=no" while not idle
    # won't work see https://github.com/the-cavalry/light-locker/issues/52

#### Screen & tty lock

I want to lock screen when going somewhere, after some time and on suspend.

_There is a storyline along tty-terminals left open, don't have that, aslong i use them and exit them immediately._

    sudo nala install xautolock xss-lock

    # contained in xmonadrc.sh
    xautolock -exit
    xautolock -time 10 -locker 'slock' -killtime 30 -killer 'systemctl suspend' -notify 10 -detectsleep &
    killall xss-lock
    xss-lock slock &

<https://wiki.archlinux.org/title/Power_management/Suspend_and_hibernate#Combined_sleep/resume_unit>

<https://www.freedesktop.org/software/systemd/man/latest/systemd.unit.html#Specifiers>

##### sflock (deprecated)

Unfortunaly I didn't get suspend work properly.

<https://github.com/benruijl/sflock>

    # git clone https://github.com/benruijl/sflock
    git clone https://github.com/julmajustus/sflock.git
    cd sflock
    sudo make clean install
    sflock -f fixed

## Candidates & Experiments

* scrot
* openvoiceos
* <https://github.com/davatorium/rofi>
* <https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-TreeSelect.html>
* xcompmgr
* clementine, streamtuner2
* shotcut
* picom

## System utilities

    sudo apt install bumblebee-nvidia

### fast terminal and console

#### fish

`run_once_1_fish_0_install.sh`
`run_once_1_fish_1completion_chezmoi.sh`
`run_once_1_fish_2fisher.sh`
`run_once_1_fish_2theme.sh`

#### wezterm

`run_once_0_wezterm_install.sh`

<https://wezterm.org/config/default-keys.html>

    wezterm show-keys

#### zutty

`run_once_0_zutty_0install.sh`

<https://tomscii.sig7.se/2020/12/A-totally-biased-comparison-of-Zutty>

#### nerd fonts

<https://gist.github.com/matthewjberger/7dd7e079f282f8138a9dc3b045ebefa0>

    pushd .
    mkdir ~/.local/share/fonts
    cd ~/.local/share/fonts
    curl -o ~/Downloads/NerdFontsSymbolsOnly.zip https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/NerdFontsSymbolsOnly.zip
    unzip ~/Downloads/NerdFontsSymbolsOnly.zip
    fc-cache -fv
    rm ~/Downloads/NerdFontsSymbolsOnly.zip
    popd

### nala

`run_once_2_nala_install.sh`

<https://github.com/volitank/nala>

### mimeapps

`mimeapps.list.tmpl`

    ln -Pfv ~/projs/xmonad.hs/.config/mimeapps.list ~/.config/mimeapps.list

### us keyboard fix

:rage: Silly time spend on this

Most startup scripts are prior to xdg-autostarts.
Keep an eye on `grep 'Exec=' /etc/xdg/autostart/*`

    sudo apt remove --purge ibus im-config # which in my case was defaulting to us-keyb

### special pipe-key combo (tablet keyboard)

    sudo cp usr/share/X11/xkb/symbols/de.chuwi_ubook_xpro /usr/share/X11/xkb/symbols/de.chuwi_ubook_xpro
    sudo cp etc/default/keyboard /etc/default/keyboard

After **reboot**, verify

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

Chuwi specific parts

    diff usr/share/X11/xkb/symbols/de.chuwi_ubook_xpro /usr/share/X11/xkb/symbols/de
        29,30c29,30
        <     key <AB01>        {[          y,          Y,              bar,          less ]}; // » › CHUWI MODIF
        <     key <AB02>        {[          x,          X,    guillemotleft,          greater ]}; // « ‹ CHUWI MODIF
        ---
        >     key <AB01>        {[          y,          Y,   guillemotright,          U203A ]}; // » ›
        >     key <AB02>        {[          x,          X,    guillemotleft,          U2039 ]}; // « ‹

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

`run_once_2_librewolf_install.sh`

Installation instructions here: <https://librewolf.net/installation/debian/>

    sudo nala install extrepo
    sudo extrepo enable librewolf
    sudo nala install librewolf
    sudo update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250

Allow history storage and whitelist a few cookies.

NOTE There is no Single-Site-Browsing / Progressive-Web-App in FF-Desktop and derivatives.

#### search engines

* <https://paulgo.io>
* <https://ecosia.helpscoutdocs.com/article/406-about-ecosia>
* <https://stract.com/about>
* <https://about.qwant.com/en/>

#### XDG default applications

Used e.g. by thunderbird

    ls ~/.local/share/applications/*
    ls /usr/share/applications/*
    xdg-settings set default-web-browser librewolf.desktop

#### Picture-in-Picture

See <https://github.com/janesser/xmonad.hs/blob/main/dot_config/xmonad/lib/FloatingVideos.hs> which allows switching floating positions by keyboard.

See <https://support.mozilla.org/en-US/kb/about-picture-picture-firefox> which denotes keyboard shortcuts for Picture-in-Picture.

Also see <https://windowsreport.com/firefox-finally-lets-you-customize-keyboard-shortcuts/> which announces a soon to land feature, enabling <about:keyboard> to adjust firefox (so also librewolf) keybindings. As `CTRL+SHIFT+]` won't work on DE QWERTZ-keyboards.

### Cloud Storage

choices

* EU-based replacement for US-based dropbox.
* should work on win, Linux, android (sync local file copy)
* need to support my flows with orgzly and keepassxc

#### nextcloud

actually trying free package at <https://www.hosting.de>

also trying to restore orgzly sync, i.e. <https://github.com/orgzly/orgzly-android/issues/33>

#### koofr DISCONTINUED

FIXME webdav integration gvfs was causing kernel halt problems

#### pcloud DISCONTINUED

FIXME had trouble with orgzly files not synced correctly

<https://www.pcloud.com/>

    # download binary from https://www.pcloud.com/how-to-install-pcloud-drive-linux.html?download=electron-64
    chmod +x ./pcloud
    sudo mv ./pcloud ~/.local/bin
    sudo apt install libfuse2t64
    pcloud # sign into account
    # find ~/pCloudDrive

## Developer Tools

### GHCup

Used for haskell-language-server. <https://www.haskell.org/ghcup/>

`run_once_2_ghcup_install.sh`

    sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
    # answer Y to haskell-language-server
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

#### Troubleshooting

* Check `ps axu|grep haskell-language-server`. Is that the version you are expecting ?
* Check `stack.yaml`. Does the version fit your ghc preferences ? Available stackages here: <https://www.stackage.org/lts>
* Check `which ghc`. Is it properly pointing to a version managed by ghcup ?

### Codium

VSCode without telemetry & tracking, see: <https://vscodium.com/#why>

`run_once_2_codium_0install.sh`

    sudo apt install extrepo
    sudo cp  /usr/share/extrepo/offline-data/debian/sid/vscodium.asc /usr/share/extrepo/offline-data/debian/trixie/
    sudo nano /usr/share/extrepo/offline-data/debian/trixie/index.yaml

        # taken from /usr/share/extrepo/offline-data/debian/sid/index.yaml
        vscodium:
        description: VS Codium repository - FLOSS binaries of VS code.
        gpg-key-checksum:
            sha256: 880957c1310498fcf1f91025fbcdf5dfb5b41832919e1f0646bcbafdb7101bd7
        gpg-key-file: vscodium.asc
        policy: main
        source:
            Architectures: amd64 i386 arm64 armhf
            Components: main
            Suites: vscodium
            Types: deb
            URIs: https://paulcarroty.gitlab.io/vscodium-deb-rpm-repo/debs/

    sudo extrepo --offlinedata enable vscodium
    ln -sf ~/projs/xmonad.hs/.config/VSCodium/User/settings.json ~/.config/VSCodium/User/settings.json

#### Extensions

`run_once_2_codium_1extensions.sh.tmpl`

    codium --list-extensions
        # outputs
        bmalehorn.vscode-fish
        davidanson.vscode-markdownlint
        dawidd6.debian-vscode
        foxundermoon.shell-format
        haskell.haskell
        jnoortheen.nix-ide
        jock.svg
        justusadam.language-haskell
        lkrms.inifmt
        mads-hartmann.bash-ide-vscode
        prince781.vala
        redhat.vscode-yaml
        tonka3000.qtvsctools
    codium --install-extension (<extension-id> | <extension-vsix-path>)
    ln -sf ~/projs/xmonad.hs/.config/VSCodium/User/settings.json ~/.config/VSCodium/User/settings.json

#### Trouble-shooting

there is no formatter for 'dockercompose' installed -> <https://github.com/redhat-developer/vscode-yaml/issues/1000>

### Jupyter

`run_once_3_devtools_4jupyterlab.sh`

    pipx install jupyterlab
    pipx inject jupyterlab jupyterlab-git
    jupyter-lab &;diswon
    # jupyter-lab stop

### ollama & deepseek

`run_once_3_devtools_4ollama.sh`

<https://www.tecmint.com/run-deepseek-locally-on-linux/>

    curl -fsSL https://ollama.com/install.sh | sh
    ollama --version # 0.5.7
    ollama run deepseek-r1:7

To uninstall, follow instructions here: <https://github.com/ollama/ollama/issues/986>

#### alternative via docker

access open-webui via <http://localhost:3000/>

    sudo podman run --rm --device nvidia.com/gpu=all ubuntu nvidia-smi -L

<https://github.com/ollama/ollama/blob/main/docs/troubleshooting.md>

<https://medium.com/@srpillai/how-to-run-ollama-locally-on-gpu-with-docker-a1ebabe451e0>

<https://podman-desktop.io/docs/podman/gpu>

<https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/latest/install-guide.html>

### StabilityMatrix

Multi-Platform Package Manager for Stable Diffusion.

<https://github.com/LykosAI/StabilityMatrix>

    sudo nala install nvidia-modprobe nvidia-cuda-toolkit
    # Download linux binary
    chmod +x StabilityMatrix.AppImage
    ## ./StabilityMatrix.AppImage --appimage-extract
    sudo mv StabilityMatrix.AppImage /usr/local/bin/

### n8n

`run_once_3_devtools_4node.sh`

can use ollama above

    npx n8n

<https://github.com/n8n-io/n8n>

### glow (shell markdown viewer)

    sudo apt install golang-go
    go install github.com/charmbracelet/glow@latest
    # PATH should expand over ~/go/bin/glow

### eclipse

<https://github.com/janesser/eclipse-snap>

### nix

    sudo nala install nix-bin
    sudo nix-channel --list
    sudo nix-channel --add  https://nixos.org/channels/nixos-24.05
    sudo nix-channel --update

<https://nix.dev/manual/nix/2.33/installation/uninstall.html>

### kubernetes

#### kubectl

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

#### k0sproject

    curl -sSLf https://get.k0s.sh | sudo sh
    k0s completion fish > ~/.config/fish/completions/k0s.fish

## Social Tools

### Signal Desktop

`run_once_2_rp_pi-apps_install.sh`

<https://www.signal.org/download/linux/>

    sudo apt install signal-desktop # signal-desktop-unofficial via pi-apps

### Whatsapp Client

#### zapzap

    `run_once_2_zapzap_install.sh`

    sudo apt install libxcb-cursor0 python3-poetry
    git clone https://github.com/rafatosta/zapzap.git
    cd zapzap
    poetry init
    poetry add PyQt6 PyQt6-WebEngine dbus-python python-gettext
    chmod +x run.py
    
    poetry run -- python3 run.py dev
    ln -sf $(pwd)/.local/bin/x-whatsapp ~/.local/bin/x-whatsapp

    git checkout 6.0.1.8
    poertry update

##### pyproject.toml

    > cat pyproject.toml 
    [tool.poetry]
    name = "zapzap"
    version = "0.1.0"
    description = ""
    authors = ["Jan Esser <JEsser@gmx.de>"]
    readme = "README.md"

    [tool.poetry.dependencies]
    python = "^3.12"
    pyqt6 = "^6.8.1"
    pyqt6-webengine = "^6.8.0"
    dbus-python = "^1.3.2"
    python-gettext = "^5.0"


    [build-system]
    requires = ["poetry-core"]
    build-backend = "poetry.core.masonry.api"

### Tuba (Mastodon Client)

    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/GeopJr/Tuba.git
    cd Tuba
    git checkout v0.8.4
    sudo apt install meson valac libjson-glib-dev libxml2-dev libgee-0.8-dev libsoup-3.0-dev libadwaita-1-dev libsecret-1-dev libgtksourceview-5-dev appstream-util
    meson setup --prefix /usr/local --reconfigure builddir/
    meson compile -C builddir/
    sudo make install

### Element Desktop (Matrix.org Client)

<https://element.io/download>

### Mail Client

    # alternatively
    sudo apt install evolution
    # or
    sudo apt install claws-mail claws-mail-vcalendar-plugin

    sudo update-alternatives --install /usr/bin/x-mail-client x-mail-client /usr/bin/evolution 50
    # sudo update-alternatives --install /usr/bin/x-mail-client x-mail-client /usr/bin/claws-mail 50
    sudo update-alternatives --install /usr/bin/x-mail-client x-mail-client /usr/bin/geary 50

### Skype 4 Linux (eol April'25)

    sudo apt remove --purge skypeforlinux
    sudo apt install snapd
    sudo snap install skype

Temporaly using <https://www.infomaniak.com/en/ksuite/kmeet>

## Games

### Steam on Linux

    XMonad.Util.Hacks.fixSteamFlicker
    https://github.com/ValveSoftware/steam-for-linux/issues/10544

    # to offload on nvidia
    __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME="nvidia" __VK_LAYER_NV_optimus="NVIDIA_only" %command% 

### Lutris

    apt install lutris
    # wip

## Deprecations

### Dismissed

Applications now deprecated and uninstalled

* stalonetray / trayer
* diodon
* snap
  * vscode
  * firefox, thunderbird
  * chromium
* flatpak
* whatsapp-for-linux

### bitseater meteo (deprecated)

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

#### WhatSie (deprecated)

CPU thrashing remained high througout several versions
<https://github.com/keshavbhatt/whatsie/issues/190>

    sudo apt install qtbase5-dev qtwebengine5-dev qtwebengine5-dev-tools
    mkdir -p ~/projs; cd ~/projs
    git clone https://github.com/keshavbhatt/whatsie.git
    cd whatsie/src
    qmake PREFIX=/usr/local
    make -j4
    sudo make install

### my-weather-indicator (deprecated)

`run_once_2_my-weather-indicator_install.sh`

<https://github.com/atareao/my-weather-indicator/blob/main/bin/my-weather-indicator>

### tmux and reptyr (deprecated)

Trying wezterm(mux) as replacement.

<https://github.com/nelhage/reptyr>

<https://tmuxcheatsheet.com/>

<https://github.com/budimanjojo/tmux.fish>

## related projects

### Homeassistant WIP

    pipx install homeassistant
    hass
    # browser access via localhost:8123

<https://community.home-assistant.io/t/device-online-offline-alert-automation/741072>

### gparted live

<https://gparted.org/livecd.php>
sudo dd bs=4M if=$HOME/Downloads/gparted-live-1.7.0-12.amd64.iso of=/dev/sdb status=progress oflag=sync

## Inspiration Sources

<https://config.phundrak.com/fish.html>
<https://mwop.net/blog/2024-07-04-how-i-use-wezterm.html>
