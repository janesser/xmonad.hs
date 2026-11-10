#!/bin/bash

command -v librewolf >/dev/null || exit 1


curl -o /tmp/keepassxc_browser.xpi https://addons.mozilla.org/firefox/downloads/file/4628286/keepassxc_browser-1.9.11.xpi
librewolf /tmp/keepassxc_browser.xpi &
