#!/bin/bash
# by this fix lightdm sources $HOME/.profile and adopts adjusted PATH variables etc.
# https://github.com/canonical/lightdm/issues/96


curl https://raw.githubusercontent.com/canonical/lightdm/refs/heads/main/debian/lightdm-session | sudo tee /usr/local/sbin/lightdm-session
sudo chmod +x /usr/local/sbin/lightdm-session
sudo sed -i 's/#session-wrapper=lightdm-session/session-wrapper=lightdm-session/' /etc/lightdm/lightdm.conf
sudo systemctl reload lightdm