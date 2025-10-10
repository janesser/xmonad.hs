#!/bin/bash
# by this fix lightdm sources $HOME/.profile and adopts adjusted PATH variables etc.
# https://github.com/canonical/lightdm/issues/96

if [ "$CHEZMOI_ARCH" = "arm64" ]
then
    curl -sL https://raw.githubusercontent.com/canonical/lightdm/refs/heads/main/debian/lightdm-session | sudo tee /usr/local/sbin/lightdm-session
    sudo chmod +x /usr/local/sbin/lightdm-session
    # enable lightdm-session as wrapper
    sudo sed -i 's/#session-wrapper=lightdm-session/session-wrapper=lightdm-session/' /etc/lightdm/lightdm.conf
    # disable autologin-user
    sudo sed -i 's/^autologin-user/#autologin-user/' /etc/lightdm/lightdm.conf
    if [ -n "$DISPLAY" ];
    then
        echo "Reboot or 'sudo systemctl restart lightdm' from outside X"
    else
        sudo systemctl restart lightdm
    fi
else
    echo $CHEZMOI_ARCH not supported for lightdm-session fix
fi