# HP Z6 G4 local AI setup

CPU: Intel(R) Xeon(R) Gold 5122 CPU @ 3.60GHz
RAM: Currently 16GB DDR4-2666 MT/s ?

https://en.wikipedia.org/wiki/List_of_Intel_Xeon_processors_(Skylake-based)#Xeon_Gold_(quad_processor)
https://www.youtube.com/watch?v=7uORDCAQ2ZI

    # forget about decode-dimms
    $ inxi -mx
        Memory:
          System RAM: total: 16 GiB available: 14.81 GiB used: 11.48 GiB (77.5%)
          Message: For most reliable report, use superuser + dmidecode.
          Array-1: capacity: 2.50 TiB note: check slots: 6 modules: 2 EC: Single-bit ECC
            max-module-size: 426.67 GiB note: est.
          Device-1: CPU0-DIMM1 type: DDR4 size: 8 GiB speed: 2133 MT/s
          Device-2: CPU0-DIMM2 type: no module installed
          Device-3: CPU0-DIMM3 type: no module installed
          Device-4: CPU0-DIMM4 type: no module installed
          Device-5: CPU0-DIMM5 type: no module installed
          Device-6: CPU0-DIMM6 type: DDR4 size: 8 GiB speed: 2133 MT/s

## software

ubuntu 26

nvidia-drivers-580 for tesla v100 (compute)

nouveau for GeForce GT 730 (display; coexists with 580 on the V100 — needs mxm_wmi loaded first)

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
