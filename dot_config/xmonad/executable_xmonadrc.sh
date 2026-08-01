#!/usr/bin/env bash

killall btop # kill dangling instances

xsetroot -solid black # feh for background image

gtk-sni-tray-standalone --bottom --beginning --watcher &

source ~/.local/share/start_once.func

start_once blueman-applet

# "systemctl suspend" cannot be passed through as quoting doesn't group args correctly
xautolock -time 10 -locker slock -killtime 30 -killer "systemctl suspend" -notify 10 -detectsleep &
xss-lock -- on-screenlock.fish &

# no over-gain mic
pactl set-source-volume @DEFAULT_SOURCE@ 20%

start_once copyq
#start_once nextcloud --background
start_once x-weather-client

# either will work on amd64 and arm64
start_once nm-applet
start_once nm-tray

### COMM ###

# x-social.sh

#### WEB ####

# x-www-browser

## ADMIN
# start_once easyeffects
start_once keepassxc
start_once pavucontrol

## BACKGROUND
~/.local/bin/check_host_online.sh airRohr-505856 &

if [ -n "$(udevadm info --export-db | grep ID_INPUT_TOUCHSCREEN)" ]; then
   # https://bugs.launchpad.net/onboard/+bug/1633284
   gsettings reset org.onboard.window.landscape x
   gsettings reset org.onboard.window.landscape y
   gsettings reset org.onboard.window.landscape width
   gsettings reset org.onboard.window.landscape height
   start_once onboard -e
fi
