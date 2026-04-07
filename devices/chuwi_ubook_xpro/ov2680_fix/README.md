# Chuwi UBook XPro S5K3_A1

## Chuwi Camera Fixing WIP

Chuwi Ubook Xpro has an (back, over screen) ov2680 and (front) ov5648.
Both work in windows 10. In vanilla linux ubuntu ov2680 is loaded, but won't result in any camera that e.g. `cheese` will detect.

Datasheet of the hardware vendor

<https://datasheet4u.com/pdf-down/O/V/2/OV2680-OmniVision.pdf>

### Context

Kerneldriver is there since early intel atom cpus.

<https://github.com/torvalds/linux/blob/master/drivers/media/i2c/ov2680.c>

Surface Driver Situation is here. Chuwi has used some hardware known there.

<https://github-wiki-see.page/m/linux-surface/linux-surface/wiki/Camera-Support>

### Symptoms

From dmesg

```sh
sudo dmesg|grep ov2680

[    6.329170] ov2680 i2c-OVTI2680:00: supply DOVDD not found, using dummy regulator
[    6.329230] ov2680 i2c-OVTI2680:00: supply DVDD not found, using dummy regulator
[    6.329244] ov2680 i2c-OVTI2680:00: supply AVDD not found, using dummy regulator
[    6.350631] ov2680 i2c-OVTI2680:00: sensor_revision id = 0x2680, rev= 0
```

From libcamera (with ipu3 loaded)

```sh
cam -l

[0:01:05.157581439] [3433]  INFO Camera camera_manager.cpp:284 libcamera v0.0.0+3-666f17af
[0:01:05.180934159] [3434]  WARN CameraSensor camera_sensor.cpp:259 'ov2680 2-0010': Recommended V4L2 control 0x009a0922 not supported
[0:01:05.181018664] [3434]  WARN CameraSensor camera_sensor.cpp:331 'ov2680 2-0010': The sensor kernel driver needs to be fixed
[0:01:05.181047134] [3434]  WARN CameraSensor camera_sensor.cpp:333 'ov2680 2-0010': See Documentation/sensor_driver_requirements.rst in the libcamera sources for more information
[0:01:05.181075706] [3434] ERROR CameraSensor camera_sensor.cpp:355 'ov2680 2-0010': Mandatory V4L2 control 0x009e0902 not available
[0:01:05.181096090] [3434] ERROR CameraSensor camera_sensor.cpp:355 'ov2680 2-0010': Mandatory V4L2 control 0x009e0901 not available
[0:01:05.181111188] [3434] ERROR CameraSensor camera_sensor.cpp:363 'ov2680 2-0010': The sensor kernel driver needs to be fixed
[0:01:05.181124754] [3434] ERROR CameraSensor camera_sensor.cpp:365 'ov2680 2-0010': See Documentation/sensor_driver_requirements.rst in the libcamera sources for more information
```

### Testing

`cheese` should start working at any point

### Discontinued approaches

### Loading ipu3_imgu

```sh
sudo dmesg|less

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
```

#### compile patched ov2680 kernel module

<https://github.com/torvalds/linux/blob/master/drivers/media/i2c/ov2680.c>

See `Makefile`

#### bios hack for hardware visibility

NO CHANGE IN PERCEIVED HARDWARE

<https://chat.z.ai/c/d866198d-1b8f-4bdf-9344-8eb66a185820>

1. Kernel cmdline `acpi_osi` Windows 2012, Windows 2022 or Linux won't make anything appear with lsusb. <https://learn.microsoft.com/en-us/windows-hardware/drivers/acpi/winacpi-osi>
1. i2c direct access <https://github.com/djrscally/miix-510-cameras>: this article describes a few i2ctransfer tricks, none has worked so far. `i2cdetect -l -r

#### dell ipu6 drivers

NO IPU6 CAMERA ARISED

<https://dev.boutantin.net/Linux/Infra/Devices/intel_mipi_camera/>

```sh
sudo add-apt-repository ppa:oem-solutions-group/intel-ipu6
sudo apt install libcamhal0
sudo apt remove --purge intel-ipu6-dkms


echo "blacklist ipu3_cio2" | sudo tee /etc/modprobe.d/blacklist-ipu3.conf
sudo update-initramfs -u
```

#### surface linux kernel

<https://chat.z.ai/c/6ae1b05b-c28d-4609-b606-ed70240f5ec8>

<https://github.com/linux-surface/linux-surface>

```bash
wget -qO - https://raw.githubusercontent.com/linux-surface/linux-surface/master/pkg/keys/surface.asc | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/linux-surface.gpg >/dev/null

echo "deb [arch=amd64] https://pkg.surfacelinux.com/debian release main" | sudo tee /etc/apt/sources.list.d/linux-surface.list

sudo apt update
sudo apt install -y linux-image-surface linux-headers-surface
```

```sh
cam -l

[0:01:41.757200908] [4543]  INFO Camera camera_manager.cpp:284 libcamera v0.0.0+3-666f17af
[0:01:41.844768036] [4548]  WARN CameraSensorProperties camera_sensor_properties.cpp:262 No static properties available for 'ov2680'
[0:01:41.845982693] [4548]  WARN CameraSensorProperties camera_sensor_properties.cpp:264 Please consider updating the camera sensor properties database
[0:01:41.965587917] [4548] ERROR IPAProxy ipa_proxy.cpp:149 Configuration file 'ov2680.yaml' not found for IPA module 'ipu3'
[0:01:41.965986849] [4548] ERROR IPAIPU3 ipu3.cpp:307 ipu3: Failed to create camera sensor helper for ov2680
[0:01:41.966163164] [4548] ERROR IPU3 ipu3.cpp:1196 Failed to initialise the IPU3 IPA
```

#### libcamera 0.7.0+

```sh
    sudo apt install \
    build-essential meson ninja-build pkg-config libgnutls28-dev openssl doxygen \
    qtbase5-dev libqt5core5a libqt5gui5 libqt5widgets5 qttools5-dev-tools \
    libtiff-dev libevent-dev libyaml-dev \
    gstreamer1.0-tools libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev
    # excluded python3-pip python3-yaml python3-ply python3-jinja2 \

    # assumed astral-uv on the system
    uv venv # create .venv in libcamera folder
    source .venv/bin/activate.fish # deactivate
    uv pip install pip # into the venv, verify which pip
    pip install ply jinja2 sphinxcontrib-doxylink sphinx-book-theme

    meson setup -Dpipelines=uvcvideo,vimc,ipu3 -Dipas=vimc,ipu3 -Dprefix=/usr/local -Dgstreamer=enabled -Dv4l2=enabled -Dbuildtype=release build/
    meson compile -C build/
    sudo meson install -C build/

    # sudo ninja uninstall -C build/
    # sudo ldconfig
```

```sh
cam -l

[4:14:38.813513058] [151724]  INFO Camera camera_manager.cpp:340 libcamera v0.7.0
[4:14:38.842158627] [151725]  WARN CameraSensorProperties camera_sensor_properties.cpp:538 No static properties available for 'ov2680'
[4:14:38.842192928] [151725]  WARN CameraSensorProperties camera_sensor_properties.cpp:540 Please consider updating the camera sensor properties database
[4:14:38.846710170] [151725]  WARN IPAProxy ipa_proxy.cpp:192 Configuration file 'ov2680.yaml' not found for IPA module 'ipu3', falling back to '/usr/local/share/libcamera/ipa/ipu3/uncalibrated.yaml'
[4:14:38.846768718] [151725] ERROR IPAIPU3 ipu3.cpp:306 ipu3: Failed to create camera sensor helper for ov2680
[4:14:38.846821989] [151725] ERROR IPU3 ipu3.cpp:1196 Failed to initialise the IPU3 IPA
Available cameras:
```

#### fix up media-ctl ?

<https://www.linuxtv.org/downloads/v4l-dvb-apis-new/pdf/media.pdf>
