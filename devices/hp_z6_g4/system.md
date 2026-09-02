# HP Z6 G4 local AI setup

## bios reset procedure

PWSD jumper
turn on
reboot

in admin-powershell

    BiosConfigUtil64 /get
    # "Admin Password Set" will be shown with "No"

## software

ubuntu 26

nvidia-drivers-580 for tesla v100

llama-cpp self-compiled with CUDA

### enable wake on lan

    nmcli connection show "Kabelgebundene Verbindung 1" |grep 802
    sudo nmcli connection modify "Kabelgebundene Verbindung 1" 802-3-ethernet.auto-negotiate yes
    sudo nmcli connection modify "Kabelgebundene Verbindung 1" 802-3-ethernet.wake-on-lan magic
    nmcli connection modify "Kabelgebundene Verbindung 1" 802-3-ethernet.wake-on-lan magic
    nmcli connection show "enp9s0f2np2" |grep 802

### firmware update

    fwupdmgr get-devices
    fwupdmgr get-updates
    fwupdmgr get-upgrades
    fwupdmgr update
