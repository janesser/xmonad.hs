#!/usr/bin/fish

if test $on_screenlock_mute = MUTE
    # MUTE all devices,
    # in case on resume DEFAULT_SINK has changed,
    # e.g. when bluetooth connection didn't persist
    for sink in $(pactl list sinks|grep -Po "(?<=Name: ).*")
        pactl set-sink-mute $sink on
    end
end

slock