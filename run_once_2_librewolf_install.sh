#!/bin/bash

sudo extrepo enable librewolf
sudo apt install librewolf
sudo update-alternatives --install /usr/bin/x-www-browser x-www-browser /usr/bin/librewolf 250