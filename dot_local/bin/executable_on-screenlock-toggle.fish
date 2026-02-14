#!/usr/bin/fish

if test $on_screenlock_mute = MUTE
    set -U on_screenlock_mute NO_MUTE
else
    set -U on_screenlock_mute MUTE
end

echo $on_screenlock_mute