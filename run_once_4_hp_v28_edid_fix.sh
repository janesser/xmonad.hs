#!/bin/bash

for HDMI in $(find /sys/class/drm -name "*HDMI*"); do
    HDMI_STATUS=`cat $HDMI/status`
    HDMI_EDID=`cat $HDMI/edid`
    if [ "$HDMI_STATUS" = "connected" ] && [ -z "$HDMI_EDID" ]
    then
        echo on $HDMI: forcing hp-v28 edid into kernel parameters
        cd {{ .chezmoi.sourceDir }}
        sudo cp raspberry/hpv28.bin /usr/lib/firmware/
        if [ -f /boot/firmware/cmdline.txt ] && ! grep drm.edid_firmware /boot/firmware/cmdline.txt
        then
            # TODO use specific $HMDI substring here
            sudo sed -i 's/$/ drm.edid_firmware=HDMI-A-1:hpv28.bin/' /boot/firmware/cmdline.txt
        fi
    fi
done