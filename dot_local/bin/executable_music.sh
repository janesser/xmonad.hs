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

# no video rendering, no on-screen display, closes when the stream ends
exec mpv --no-video --no-osc --no-keep-open "$URL"
