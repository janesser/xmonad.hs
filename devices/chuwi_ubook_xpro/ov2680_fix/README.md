# Chuwi UBook XPro S5K3_A1

## Chuwi Camera Fixing WIP

Chuwi Ubook Xpro has an (back, over screen) ov2680 and (front) ov5648.
Both work in windows 10. In vanilla linux ubuntu ov2680 is loaded, but won't result in any camera that e.g. `cheese` will detect.

Datasheet of the hardware vendor

<https://datasheet4u.com/pdf-down/O/V/2/OV2680-OmniVision.pdf>

Kerneldriver is there since early intel atom cpus.

<https://github.com/torvalds/linux/blob/master/drivers/media/i2c/ov2680.c>

### Symptoms

From dmesg

    [    6.329170] ov2680 i2c-OVTI2680:00: supply DOVDD not found, using dummy regulator
    [    6.329230] ov2680 i2c-OVTI2680:00: supply DVDD not found, using dummy regulator
    [    6.329244] ov2680 i2c-OVTI2680:00: supply AVDD not found, using dummy regulator
    [    6.350631] ov2680 i2c-OVTI2680:00: sensor_revision id = 0x2680, rev= 0

From libcamera (with ipu3 loaded)

    cam -l
    [0:01:05.157581439] [3433]  INFO Camera camera_manager.cpp:284 libcamera v0.0.0+3-666f17af
    [0:01:05.180934159] [3434]  WARN CameraSensor camera_sensor.cpp:259 'ov2680 2-0010': Recommended V4L2 control 0x009a0922 not supported
    [0:01:05.181018664] [3434]  WARN CameraSensor camera_sensor.cpp:331 'ov2680 2-0010': The sensor kernel driver needs to be fixed
    [0:01:05.181047134] [3434]  WARN CameraSensor camera_sensor.cpp:333 'ov2680 2-0010': See Documentation/sensor_driver_requirements.rst in the libcamera sources for more information
    [0:01:05.181075706] [3434] ERROR CameraSensor camera_sensor.cpp:355 'ov2680 2-0010': Mandatory V4L2 control 0x009e0902 not available
    [0:01:05.181096090] [3434] ERROR CameraSensor camera_sensor.cpp:355 'ov2680 2-0010': Mandatory V4L2 control 0x009e0901 not available
    [0:01:05.181111188] [3434] ERROR CameraSensor camera_sensor.cpp:363 'ov2680 2-0010': The sensor kernel driver needs to be fixed
    [0:01:05.181124754] [3434] ERROR CameraSensor camera_sensor.cpp:365 'ov2680 2-0010': See Documentation/sensor_driver_requirements.rst in the libcamera sources for more information

### Testing

`cheese` should start working at any point

### Loading ipu3_imgu

    sudo apt install intel-ipu6-dkms

        [    6.575992] ipu3-cio2 0000:00:14.3: Found supported sensor OVTI2680:00
        [    6.576548] ipu3-cio2 0000:00:14.3: Connected 1 cameras
        [    6.576567] ipu3-cio2 0000:00:14.3: enabling device (0000 -> 0002)
        [    6.579618] i2c i2c-3: 2/2 memory slots populated (from DMI)
        [    6.579711] ipu3-cio2 0000:00:14.3: device 0x9d32 (rev: 0x1)
        [    6.581238] ov2680 i2c-OVTI2680:00: supply DOVDD not found, using dummy regulator
        [    6.581303] ov2680 i2c-OVTI2680:00: supply DVDD not found, using dummy regulator
        [    6.581317] ov2680 i2c-OVTI2680:00: supply AVDD not found, using dummy regulator

    # loaded automatically on boot
    sudo modprobe ipu3_imgu
    sudo modprobe ipu3_cio2

### Didn't work (yet?)

<https://chat.z.ai/c/d866198d-1b8f-4bdf-9344-8eb66a185820>

1. Kernel cmdline `acpi_osi` Windows 2012, Windows 2022 or Linux won't make anything appear with lsusb. <https://learn.microsoft.com/en-us/windows-hardware/drivers/acpi/winacpi-osi>
1. i2c direct access <https://github.com/djrscally/miix-510-cameras>: this article describes a few i2ctransfer tricks, none has worked so far. `i2cdetect -l -r
