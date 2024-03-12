# rhasspy

Trying rhasspy (after openvoiceos) <https://github.com/rhasspy/rhasspy>

Ubuntu 22

* podman
* pipewire

## plain debian

gfortran4 issue <https://github.com/rhasspy/rhasspy/issues/322>

## rhasspy with pipewire

Container won't play any sound on host with pipewire

<https://stackoverflow.com/questions/68973199/pipewire-audio-in-fedora-container>

    # podman docker emulation
    docker run --rm  -p 12101:12101 \
        --name rhasspy \
        -v "$HOME/.config/rhasspy/profiles:/profiles" \
        -v "/etc/localtime:/etc/localtime:ro" \
        -v /run/user/1000/pipewire-0:/tmp/pipewire-0 \
        -e XDG_RUNTIME_DIR=/tmp \
        --device /dev/snd:/dev/snd \
        rhasspy/rhasspy \
        --user-profiles /profiles \
        --profile de

    podman exec -it rhasspy bash
    
        mkdir -p /var/cache/apt/amd64/archives/partial
        apt install pipewire

    no pipewire-alsa in buster

### building pipewire enhanced docker

#### missing dependencies

    NOBUILDX=no ./scripts/build-docker.sh

    # [1/2] STEP 11/54: COPY download/shared/ ${DOWNLOAD_DIR}/shared/
    # Error: building at STEP "COPY download/shared/ ${DOWNLOAD_DIR}/shared/": checking on sources under "/home/jan/projs/rhasspy": copier: stat: "/download/shared": no such file or directory

#### unavailable dependencies

    ./scripts/download-dependencies.sh
    
    # --2024-02-03 15:09:59--  https://github.com/synesthesiam/prebuilt-apps/releases/download/v1.0/kenlm-20200308_armv7.tar.gz
    2024-02-03 15:09:59 FEHLER 404: Not Found.

<https://github.com/synesthesiam/prebuilt-apps/releases/download/v1.0/> no kenlm here
