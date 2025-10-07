#!/bin/bash

if command -v add-apt-repository >/dev/null 2>&1
then
    sudo add-apt-repository ppa:atareao/atareao
    sudo apt install my-weather-indicator
else
    echo add-apt-repository not found to enable my-weather-indicator PPA
fi