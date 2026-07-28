# Chuwi Ubook XPro Camera Driver

## Overview
A V4L2 PCI driver for the Chuwi Ubook XPro camera modules (CAM0: IMX135, CAM1: OV2740).

Presently in `dmesg`

   i2c i2c-OVTI2680:00: deferred probe pending: ov2680: waiting for fwnode graph endpoint

<https://www.kernel.org/doc/html/v4.13/media/kapi/v4l2-fwnode.html>

## Hardware Details (from DSDT)
- **CAM0 (IMX135-CRDG2):** INT3471, I2C2 bus (PCI0.I2C2), I2C address 0x0010
  - Depends on PMIC (INT3472) at I2C address 0x004C on I2C2
  - PMIC power sequencing required before sensor init
- **CAM1 (OV2740-CRDG2):** INT3474, I2C4 bus (PCI0.I2C4), I2C address 0x0036
  - Depends on I2C2.PMIC for power
- **PMIC (PMIC-CRDG2):** INT3472, I2C2 bus, I2C address 0x004C

## Architecture
- Multi-device abstraction (`camera_device` struct)
- V4L2 video capture framework with vb2 queue
- PMIC power control for CAM0
- Sensor-specific initialization functions:
  - `imx135_init()` - IMX135 sensor initialization
  - `ov2740_init()` - OV2740 sensor initialization

## Building
```bash
make
```

## Usage
```bash
sudo insmod chuwi_camera_driver.ko
ls /dev/video*
```

## Files
- `chuwi_camera_driver.c` - Main driver source
- `Makefile` - Build configuration
- `dsdt.cam0_cam1.dsl` - ACPI DSDT for CAM0/CAM1 resources
