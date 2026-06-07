#!/bin/bash

if command -v add-apt-repository >/dev/null 2>&1
then
    # removal after disfunction FIXME find replacement
    sudo add-apt-repository -y -r ppa:atareao/atareao
    sudo apt remove --purge -y my-weather-indicator
else
    echo add-apt-repository not found.
fi

sudo apt install -y meteo-qt

sudo update-alternatives --install /usr/bin/x-weather-client x-weather-client $(which meteo-qt) 50
