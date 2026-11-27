# Lenovo Yoga 510-14AST Laptop (ideapad) - Type 80S9 installation

It is a [AMD A9-9410](https://cpu-benchmark.org/cpu/amd-a9-9410/) Soc with integrated R5 STONEY. R5 Stoney has 2 pipelines working with 192 shader units.

<https://wiki.archlinux.org/title/AMDGPU>

## Maintenance manual

<https://download.lenovo.com/consumer/mobiles_pub/yoga_510-14ast_510-14isk_510-15isk_510-14ikb_510-15ikb_hmm_201608.pdf>

## Secure Boot

Every boot waits 1m30s until awaiting /dev/tpm0 times out.

Follow instructions from <https://wiki.cachyos.org/de/configuration/secure_boot_setup/>

https://github.com/systemd/systemd/issues/33412#issuecomment-2557737935

    sudo dmesg|grep -i tmp
    # tpm_crb_acpi MSFT0101:00: error -EBUSY: can't request region for resource [mem 0xdfb76000-0xdfb79fff]
    sudo less /proc/iomem
    # !! acpi from dsdt overlaps which is visible df87f < dfb7d < dfb7e
    # df87f000-dfb7efff : ACPI Non-volatile Storage
    # dfb76000-dfb79fff : MSFT0101:00
    # dfb7a000-dfb7dfff : MSFT0101:00
    sudo nano /etc/default/limine
    # add "memmap=0x8000%0xdfb76000-4" to kernel-args

## Secure Boot

Every boot waits 1m30s until awaiting /dev/tpm0 times out OUTDATED

Follow instructions
https://wiki.cachyos.org/de/configuration/secure_boot_setup/

    sudo sbctl status # setup mode is required, otherwise return to BIOS
    sudo sbctl create-keys
    sudo sbctl enroll-keys --microsoft --firmware-builtin
    sudo limine-enroll-config
    sudo limine-update

## Known issues

### fan OUTDATED

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

#### OUTDATED Fix: Enable auto performance for amd SoC

    cat /sys/devices/pci0000:00/0000:00:01.0/power_dpm_force_performance_level # should be "auto"
    # echo "auto" | sudo tee /sys/devices/pci0000:00/0000:00:01.0/power_dpm_force_performance_level
    watch -n 0.5 cat /sys/devices/pci0000:00/0000:00:01.0/pp_dpm_sclk

### btop shows no gpu OUTDATED

rocm-smi won't work for STONEY chipset.

as workaround:

Using amdgpu_top instead makes it two top commands to watch.

    sudo snap install rustup
    rustup default stable
    cargo install amdgpu_top
    # ~/.cargo/bin/amdgpu_top needs to be on PATH
