#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pushd .
cd $SCRIPT_DIR

mkdir -p ~/ovos/config
cp config/* ~/ovos/config/

mkdir -p ~/projs
cd ~/projs
git clone https://github.com/OpenVoiceOS/ovos-docker.git
cd ovos-docker/compose
cp .env.example .env
podman-compose -f docker-compose.yml -f docker-compose.skills.yml up -d

# for german use mycroft.conf and audio.list

# podman-compose -f docker-compose.yml -f docker-compose.skills.yml logs ovos_core
# podman-compose -f docker-compose.yml -f docker-compose.skills.yml logs ovos_audio

# podman-compose -f docker-compose.yml -f docker-compose.skills.yml exec ovos_core bash
## try ovos-config, ovos-simple-cli

# podman-compose -f docker-compose.yml -f docker-compose.skills.yml exec ovos_audio bash
## try ovos-speak

# podman system prune -a -f

popd