#!/bin/bash

sudo cp  /usr/share/extrepo/offline-data/debian/sid/vscodium.asc /usr/share/extrepo/offline-data/debian/trixie/
echo << EOF
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
EOF | sudo tee -a /usr/share/extrepo/offline-data/debian/trixie/index.yaml

sudo extrepo --offlinedata enable vscodium
sudo apt install codium
