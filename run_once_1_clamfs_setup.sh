#!/bin/bash

# https://www.scribd.com/document/62059208/Clamfs-Module
if pushd ~/.downloads
then
    echo "clamfs ~/.downloads already prepared."
else
    mv ~/Downloads ~/.downloads
    mkdir ~/Downloads

    clamfs ~/.config/clamfs.d/downloads.xml
fi