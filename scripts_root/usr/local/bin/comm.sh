#!/bin/sh
if [ ! "$(pidof stalonetray)" ]; then
    thunderbird &
fi

if [ ! "$(pidof signal-desktop)" ]; then
    signal-desktop &
fi

# whatsapp
if [ ! "$(xwininfo -root -tree | grep 'WhatsApp Web')" ]; then
    dex .local/share/applications/chrome-hnpfjngllnobngcgfapefoaidbinmjnm-Default.desktop &
fi

# elements
if [ ! "$(xwininfo -root -tree | grep 'Element')" ]; then
    dex .local/share/applications/chrome-ejhkdoiecgkmdpomoahkdihbcldkgjci-Default.desktop &
fi

# mastodon det.social
if [ ! "$(xwininfo -root -tree | grep 'det.social')" ]; then
    dex .local/share/applications/chrome-hbpeoicanfoeibcjgkepjpbkdmelgaem-Default.desktop &
fi
