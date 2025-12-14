#!/bin/bash

sudo apt install nextcloud-desktop

## remove pcloud and koofr
rm -fR ~/.pcloud
rm ~/.local/bin/pcloud

rm -fR ~/.koofr
rm -fR ~/.koofr-dist