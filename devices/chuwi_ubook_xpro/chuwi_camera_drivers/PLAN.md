# Chuwi Ubook XPro Camera Driver Plan

## 1. Goal
Adapt the generic V4L2 PCI skeleton driver to support two hardware camera modules (CAM0: IMX135, CAM1: OV2740) with proper hardware identification, I2C resource management, and sensor initialization.

## 2. Hardware Topology (from DSDT)
- **CAM0 (IMX135-CRDG2):** INT3471, I2C2 bus, I2C addr 0x0010, depends on PMIC
- **CAM1 (OV2740-CRDG2):** INT3474, I2C4 bus, I2C addr 0x0036, depends on I2C2.PMIC
- **PMIC (PMIC-CRDG2):** INT3472, I2C2 bus, I2C addr 0x004C

## 3. Implementation Status

### ✅ Completed
- **Device Abstraction:** `camera_device` struct with multi-device support
- **V4L2 Framework:** Video device and vb2 queue fully initialized
- **PMIC Power Control:** `pmic_check_and_enable()` for CAM0
- **Sensor Initialization:** `imx135_init()` and `ov2740_init()` implemented
- **Hardware Identification:** PCI device ID matching in `skeleton_probe`
- **I2C Resource Acquisition:** Proper I2C adapter lookup using bus IDs
- **PCI Table:** Updated with IMX135/OV2740 PCI device IDs

### 🚧 TODO (Next Steps)
1. **Refine IMX135 register map:** The current init uses basic register writes. Full implementation would include all timing, gain, and white balance registers from the IMX135 datasheet.
2. **Refine OV2740 register map:** The current init sets up basic streaming mode. Full implementation would include all ISP configuration, test patterns, and advanced controls.
3. **ACPI parsing:** Implement DSDT parsing for more robust device identification (currently relies on PCI IDs).
4. **DMA engine integration:** Connect the vb2 queue to the actual DMA engine for frame capture.
5. **Interrupt handling:** Implement full interrupt-driven frame capture.
6. **GPIO control:** Add GPIO-based camera control (privacy shutter, focus, etc.).

## 4. Key Functions
- `imx135_init()` - IMX135 sensor initialization (chip ID, firmware, mode, timing)
- `ov2740_init()` - OV2740 sensor initialization (chip ID, MIPI, streaming mode, timing)
- `pmic_check_and_enable()` - PMIC power state check and enable
- `imx135_pmic_read/write()` - PMIC I2C communication
- `ov2740_read_reg/write_reg()` - OV2740 register access

## 5. References
- DSDT: `./dsdt.cam0_cam1.dsl`
- OV2740 kernel driver: `./linux-source-6.8.0/drivers/media/i2c/ov2740.c`
- INT3472 PMIC driver: `./linux-source-6.8.0/drivers/platform/x86/intel/int3472/`
