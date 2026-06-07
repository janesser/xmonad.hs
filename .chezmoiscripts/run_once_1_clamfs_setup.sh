#!/bin/bash

sudo apt install -y clamfs

# https://www.scribd.com/document/62059208/Clamfs-Module
if pushd ~/.downloads
then
    echo "clamfs ~/.downloads already prepared."
else
    mv ~/Downloads ~/.downloads
    mkdir ~/Downloads

    clamfs ~/.config/clamfs.d/downloads.xml
    echo clamfs activated.
    return 0
fi