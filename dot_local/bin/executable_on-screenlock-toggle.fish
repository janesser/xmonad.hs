#!/usr/bin/fish

argparse h/help m/mute u/unmute t/toggle -- $argv
or return

if set -ql _flag_h
    echo h/help m/mute u/unmute t/toggle
    return
else if set -ql _flag_m
    set -U on_screenlock_mute MUTE
else if set -ql _flag_u
    set -U on_screenlock_mute UNMUTE
else if set -ql _flag_t
    if set -qU on_screenlock_mute
        if test $on_screenlock_mute = MUTE
            set -U on_screenlock_mute UNMUTE
        else
            set -U on_screenlock_mute MUTE
        end
    else
        set -U on_screenlock_mute MUTE
    end
end

echo $on_screenlock_mute
