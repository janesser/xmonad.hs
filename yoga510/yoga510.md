# Lenovo Yoga 510-14AST Laptop (ideapad) - Type 80S9 installation

It is a [AMD A9-9410](https://cpu-benchmark.org/cpu/amd-a9-9410/) Soc with integrated R5 STONEY. R5 Stoney has 2 pipelines working with 192 shader units.

<https://wiki.archlinux.org/title/AMDGPU>

## Known issues

### fan

The A9 takes clocking to the edge which results in the fan, responding to almost any interaction.

#### Fix: Use amdgpu-dkms (proprietory)

    # download from https://www.amd.com/en/support/download/linux-drivers.html
    amdgpu-install -usecase=graphics


    # for total fan silence
    cpufreq-setall.sh powersave

Fan noise is better with amdgpu, still lowpower profile comes with perceivable performance impacts. Apparently SoC clocking is kind of joint.

### amdgpu-dkms black screen during boot

`nomodeset` as kernel param works fine.
It is backlight badly set.

#### Fix: backlight

to validate use

    # this will maintain backlight as setup by bios
    sudo systemctl mask systemd-backlight@backlight:amdgpu_bl1
    sudo systemctl unmask systemd-backlight@backlight:amdgpu_bl1

otherwise

    pkexec xfpm-power-backlight-helper --get-max-brightness # 65535
    pkexec xfpm-power-backlight-helper --set-brightness 33333
    # when brightness was set once, reboot should be safe

### video playback performance

videos playback will take more than two seconds to start and may be rambling during playback.

e.g. on youtube, playback was fine, though pre-buffering fell short several times.

#### Fix: Enable auto performance for amd SoC

    cat /sys/devices/pci0000:00/0000:00:01.0/power_dpm_force_performance_level # should be "auto"
    # echo "auto" | sudo tee /sys/devices/pci0000:00/0000:00:01.0/power_dpm_force_performance_level
    watch -n 0.5 cat /sys/devices/pci0000:00/0000:00:01.0/pp_dpm_sclk


### btop shows no gpu

rocm-smi won't work for STONEY chipset.

as workaround:

Using amdgpu_top instead makes it two top commands to watch.

    sudo snap install rustup
    rustup default stable
    cargo install amdgpu_top
    # ~/.cargo/bin/amdgpu_top needs to be on PATH
