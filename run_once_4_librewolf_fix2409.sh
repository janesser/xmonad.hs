#!/bin/bash

# https://codeberg.org/librewolf/issues/issues/2409

echo """# This profile allows everything and only exists to give the
# application a name instead of having the label "unconfined"
abi <abi/4.0>,
include <tunables/global>
profile librewolf /usr/share/librewolf/{librewolf,librewolf-bin} flags=(unconfined) {
        userns,
        # Site-specific additions and overrides. See local/README for details.
        include if exists <local/librewolf>
}
""" | sudo tee /etc/apparmor.d/librewolf

if sudo systemctl is-active apparmor >/dev/null 2>&1
then
    sudo systemctl reload apparmor
fi