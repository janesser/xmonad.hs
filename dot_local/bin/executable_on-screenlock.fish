#!/usr/bin/fish

if test $on_screenlock_mute = MUTE
    pactl set-sink-mute @DEFAULT_SINK@ on
end

slock