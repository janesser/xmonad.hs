#!/usr/bin/env bash

start_once() {
   echo starting... $1
   LINK=$(which $1)
   PRG=$(readlink -f $LINK) # dive behind x-www-browser etc.
   ARGS="${*:2}"
   if [ -x $(command -v $PRG) ] && [ ! "$(pidof $PRG)" ]; then
      exec $PRG $ARGS &
   fi
}

killall btop # kill dangling instances

xsetroot -solid black # feh for background image

start_once gtk-sni-tray-standalone --bottom --beginning --watcher
sleep 2
start_once blueman-applet

# "systemctl suspend" cannot be passed through as quoting doesn't group args correctly
xautolock -time 10 -locker slock -killtime 30 -killer "systemctl suspend" -notify 10 -detectsleep &
xss-lock -- sh -c "pactl set-sink-mute @DEFAULT_SINK@ on; slock" &

# actually without "-t" tapping isn't blocked
start_once syndaemon "-i 2 -d -K -t -m 50"

## ssh-agent
echo "# GENERATED FILE" >~/.ssh/env
killall ssh-agent
ssh-agent -c >>~/.ssh/env

# no over-gain mic
pactl set-source-volume @DEFAULT_SOURCE@ 20%

start_once copyq
start_once nextcloud
start_once /opt/extras.ubuntu.com/my-weather-indicator/bin/my-weather-indicator

# either will work on amd64 and arm64
start_once nm-applet
start_once nm-tray

### COMM ###

start_once x-mail-client
start_once signal-desktop
start_once signal-desktop-unofficial # arm64 packaging
start_once x-whatsapp
# start_once element-desktop
# INSUFFICIENT_USE start_once dev.geopjr.Tuba # compiled from github

## ADMIN
# start_once easyeffects
start_once keepassxc
start_once pavucontrol

## BACKGROUND
~/.local/bin/check_host_online.sh airRohr-505856 &

if [ -n "$(udevadm info --export-db | grep ID_INPUT_TOUCHSCREEN)" ]; then
   start_once onboard -e # FIXME rendering onboard in xmonad won't reliably work
fi
