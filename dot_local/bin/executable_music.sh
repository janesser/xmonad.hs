#!/bin/bash
## stream youtube audio in mpv with minimal battery draw

URL="$1"
if [[ -z "$URL" ]]
then
    # no argument: take the url from the clipboard (wayland)
    URL=$(wl-paste --no-newline 2>/dev/null || true)
fi

# sanity check before firing up the player
if [[ -z "$URL" ]] || [[ ! "$URL" =~ ^https://(www\.)?youtube\.com/ ]]; then
    echo "Usage: music.sh <url>          # or run with a youtube url on your clipboard"
    exit 1
fi

MPV="mpv"
if command -v flatpak; then
    if flatpak info io.mpv.Mpv; then
        MPV="flatpak run io.mpv.Mpv"
    fi
fi

# no video rendering, no on-screen display, closes when the stream ends
exec $MPV --no-video --no-osc --no-keep-open "$URL"
