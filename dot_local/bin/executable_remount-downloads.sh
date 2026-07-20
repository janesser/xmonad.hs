#!/bin/bash

fusermount -u /home/$USER/Downloads
clamfs ~/.config/clamfs.d/downloads.xml
