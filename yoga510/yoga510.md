# Lenovo Yoga 510 installation

it is a AMD A9 Soc with integrated R5 STONEY.
R5 Stoney should have 2 pipelines working with 192 shader units.

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

In `dmesg` unclear if firmware is loaded or not.
"stoney not supported by kfd" is a very famous search term.

`Xorg` logs seem just fine. Also `glxinfo` reports direct rendering enabled.
