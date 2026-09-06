# Chuwi Ubook XPro Camera Driver

> ## ⚠ This project documents the stock Intel camera stack (not a custom driver)
>
> The two Chuwi Ubook XPro cameras are **OV2680 (front)** and **OV5648 (rear)** —
> OmniVision sensors with ACPI HIDs `OVTI2680` / `OVTI5648` (confirmed from the
> Windows driver kit and live boot). This repo reverse-engineers the **stock Intel
> IPU3/CIO2 + ACPI-I2C-sensor** path (mainline drivers: `ov2680`, `ov5648`,
> `ipu3-cio2`, `int3472-tps68470`). It does **not** build a custom V4L2-PCI driver;
> the driver work is to make the stock stack bind via an ACPI/DSDT fix. See
> `agent_work/INT3472-linux-analysis.md` and `agent_work/DSDT_PATCH_PROPOSAL.md`.

## Overview
The Chuwi Ubook XPro camera stack is the stock Intel IPU3 / CIO2 + ACPI-I2C-sensor
path. CAM0 is OV2680 (front) and CAM1 is OV5648 (rear). The goal is to make the
mainline drivers bind — an ACPI/DSDT wiring fix, not a new driver.

Presently in `dmesg`

   i2c i2c-OVTI2680:00: deferred probe pending: ov2680: waiting for fwnode graph endpoint

<https://www.kernel.org/doc/html/v4.13/media/kapi/v4l2-fwnode.html>

## Hardware Details (from DSDT)
- **CAM0 (OV2680-CRDG2):** INT3471, I2C2 bus (PCI0.I2C2), I2C address 0x0010
  - Depends on PMIC (INT3472) at I2C address 0x004C on I2C2
  - PMIC power sequencing required before sensor init
- **CAM1 (OV5648-CRDG2):** INT3474, I2C4 bus (PCI0.I2C4), I2C address 0x0036
  - Depends on I2C2.PMIC for power
- **PMIC (PMIC-CRDG2):** INT3472, I2C2 bus, I2C address 0x004C

## Architecture
- Multi-device abstraction (`camera_device` struct)
- V4L2 video capture framework with vb2 queue
- PMIC power control for CAM0
- Sensor-specific initialization functions:
  - `OV2680_init()` - OV2680 sensor initialization
  - `OV5648_init()` - OV5648 sensor initialization

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
