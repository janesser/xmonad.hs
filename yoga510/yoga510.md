# Lenovo Yoga 510 installation

it is a [AMD A9-9410](https://cpu-benchmark.org/cpu/amd-a9-9410/) Soc with integrated R5 STONEY. R5 Stoney has 2 pipelines working with 192 shader units.

## Known issues

## video playback performance

videos playback will take more than two seconds to start and may be rambling during playback.

e.g. on youtube, playback was fine, though pre-buffering fell short several times.

### fan

The A9 takes clocking to the edge which results in the fan, responding to almost any interaction.

    # workaround
    cpufreq-setall.sh powersave

### amdgpu

No GPU monitoring by btop.
rcm-smi won't recognize this hardware.

#### Diagnosis

`nomodeset` as kernel param works fine.
It is backlight badly set.

to validate use

    sudo systemctl unmask systemd-backlight@backlight:amdgpu_bl1

otherwise

    pkexec xfpm-power-backlight-helper --get-max-brightness # 65535
    pkexec xfpm-power-backlight-helper --set-brightness 33333
    # once brightness was set reboot should be safe
