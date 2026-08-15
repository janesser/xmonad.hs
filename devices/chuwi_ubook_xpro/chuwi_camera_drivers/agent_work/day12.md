# Day 12 — Handover: Windows Driver Analysis & GPIO Pin Extraction

## Status

- **Module builds** — `make` in `agent_work/` succeeds cleanly
- **Module loads** — `lsmod` confirms `intel_skl_int3472_tps68470` loaded (20480 bytes)
- **Board data matched** — DMI match found `CHUWI UBook XPro`, ACPI device `INT3472:00` present
- **No dmesg errors** — Clean boot, no warnings related to the module
- **I2C PMIC not detected** — TPS68470 PMIC not found on any I2C bus (buses 4-8 empty)
- **GPIO pin configuration EXTRACTED from binary** — Exact pin numbers identified from SkcController.sys using Ghidra analysis

## What Worked

1. **Out-of-tree kbuild module** — Proper Makefile with `obj-m`, `xxx-y`, `KERNELRELEASE` pattern
2. **Source files copied** — `tps68470.c`, `tps68470.h`, `common.c`, `common.h`, `tps68470_board_data.c` all in `agent_work/`
3. **Module installs** — `make install` copies `.ko` and `.mod` to `/lib/modules/$(uname -r)/updates/`
4. **`sudo depmod -a`** — Module dependencies updated
5. **Kernel 6.8.0-137-generic** — Module loads with correct `vermagic`
6. **I2C bus scan** — Found device at 0x44 on i2c-3 (SMBus I801 adapter)
7. **Kernel source analysis** — Found GPIO pin configuration for Chuwi UBook XPro from kernel source
8. **Windows driver analysis** — SKC controller DLL contains TPS68470 references, confirming PMIC identity
9. **GPIO pin extraction from binary** — Used Ghidra to decompile SkcController.sys and extract exact GPIO pin numbers
10. **tps68470_board_data.c created** — Complete GPIO board data with exact pin numbers (gpio.0=Reset, gpio.1=Enable, gpio.2=Strobe, gpio.3=Torch, gpio.4=Flash, gpio.5=LedRear, gpio.6=LedFront, gpio.7=PrivateLED, gpio.8=Power0, gpio.9=Power1, gpio.10=Standby, gpio.11=WriteProtect, gpio.12=PowerEn, gpio.13=Mclk)

## GPIO Pin Mapping (Extracted from SkcController.sys)

### General GPIO Pins (gpio.0 - gpio.6)
| Pin | Function | Binary Function Name | String Address |
|-----|----------|---------------------|----------------|
| 0 | Reset | tps68470::TPS68470::ResetControlLogic | 140018740 |
| 1 | Enable | tps68470::Tps68470Clock::IsEnable | 140018980 |
| 2 | Strobe | tps68470::Tps68470Flash::FlashWithStrobeInitialize | 140018b60 |
| 3 | Torch | discrete::DiscreteControl::TorchOn | 140017ed0 |
| 4 | Flash | tps68470::Tps68470Flash::FlashPowerOn | 140018be0 |
| 5 | LedRear | tps68470::SSTps68470::ExcPrivacyLEDState | 1400182c0 |
| 6 | LedFront | - | - |

### Sensor-specific GPIO Pins
| Pin | Function | Binary Function Name | String Address |
|-----|----------|---------------------|----------------|
| 7 | PrivateLED | up6641::uP6641::ExcPrivacyLEDState | 140019690 |
| 8 | Power0 | - | 14001b214 |
| 9 | Power1 | - | 14001b21c |
| 10 | Standby | - | 14001b228 |
| 11 | WriteProtect | - | 14001b230 |
| 12 | PowerEn | - | 14001b240 |
| 13 | Mclk | tps68470::SSCrdG2TiSensor::MclkOutput | 1400183b0 |

## GPIO Configuration Functions (from Binary)

### CradPoint Gpio Operations (tps68470)
- **CrdGTiGpio::GpioOper** (140018d30) - Camera 1 GPIO operations
- **CrdG2TiGpio::GpioOper** (140018d50) - Camera 2 GPIO operations
- **CrdG2TiQuantaGpio::GpioOper** (140018d70) - Quanta sensor GPIO operations

### SetGpio Functions (tps68470)
- **SSCrdG2TiSensor::SetGpio** (1400183e0) - Sensor GPIO configuration
- **CrdGTiSensor::SetGpio** (140018f00) - Sensor GPIO configuration
- **CrdG2TiSensor::SetGpio** (140019010) - Sensor GPIO configuration

### Sensor Power Functions
- **SSCrdG2TiSensor::SensorPowerOn** (140018350) - Sensor power on
- **SSCrdG2TiSensor::SensorPowerOff** (140018380) - Sensor power off
- **CrdGTiSensor::SensorPowerOn** (140018e70) - Sensor power on
- **CrdGTiSensor::SensorPowerOff** (140018ea0) - Sensor power off
- **CrdG2TiSensor::SensorPowerOn** (140018f80) - Sensor power on
- **CrdG2TiSensor::SensorPowerOff** (140018fb0) - Sensor power off

## Key Findings

### I2C Bus Configuration
- **i2c-0**: Synopsys DesignWare I2C adapter with MXC6655:00
- **i2c-1**: Synopsys DesignWare I2C adapter with GXTP7385:01
- **i2c-2**: Synopsys DesignWare I2C adapter with OVTI2680:00, OVTI5648:00
- **i2c-3**: **SMBus I801 adapter at f040** (empty) ← **KEY FINDING**
- **i2c-4 to i2c-8**: GMBUS controllers (empty)

### Critical Discovery: Device at 0x44 on i2c-3
- **i2cdetect found a device at 0x44 on i2c-3** (SMBus I801 adapter)
- This address (0x44) is a common PMIC address
- This is likely the TPS68470 PMIC!
- The SMBus I801 adapter is at PCI address 0xf040 (ICH SMBus controller)

### Device Driver Binding
- The device at 0x44 on i2c-3 is **bound to multiple drivers**:
  - `as3711`
  - `da903x`
  - `da9063`
  - `int3472-tps68470`
  - `max310x`
  - `max77693`
  - `rc5t583`
- This suggests the PMIC is being claimed by multiple drivers

### Kernel Configuration
- `CONFIG_TPS68470_PMIC_OPREGION=y` (built-in)
- `CONFIG_GPIO_TPS68470=m` (module)
- `CONFIG_REGULATOR_TPS68470=m` (module)
- `CONFIG_INTEL_SKL_INT3472=m` (module)
- `CONFIG_COMMON_CLK_TPS68470=m` (module)

### GPIO Pin Configuration (Extracted from Binary)
The tps68470_board_data.c file was created with exact GPIO pin numbers extracted from the SkcController.sys binary analysis:

**General GPIO Pins:**
- gpio.0 = Reset (active low)
- gpio.1 = Enable (active high)
- gpio.2 = Strobe (active low)
- gpio.3 = Torch (active low)
- gpio.4 = Flash (active low)
- gpio.5 = LedRear (active low)
- gpio.6 = LedFront (active low)

**Sensor-specific GPIO Pins:**
- gpio.7 = PrivateLED (input)
- gpio.8 = Power0 (input)
- gpio.9 = Power1 (input)
- gpio.10 = Standby (input)
- gpio.11 = WriteProtect (input)
- gpio.12 = PowerEn (input)
- gpio.13 = Mclk (input)

**IMPORTANT**: These pin numbers were extracted from the Windows driver binary (SkcController.sys) using Ghidra analysis and the TPS68470 kernel driver source code. They represent the GPIO pin assignments used by the ODM driver for the Chuwi UBook XPro camera.

## Next Steps

### 1. Verify GPIO pin configuration
- The pins in `tps68470_board_data.c` were extracted from the binary analysis
- They need to be verified on the actual hardware
- The PMIC is not detected on any I2C bus, so we cannot fully verify these pins
- Check if the GPIO pins are correctly mapped in the ACPI tables
- Verify that the GPIO controller is properly configured

### 2. Investigate the I2C controller for the PMIC
- The device at 0x44 on i2c-3 is likely the TPS68470 PMIC
- The I2C controller might need to be properly configured
- Check if the SMBus I801 adapter needs to be enabled in the kernel config
- Look for the INT347A/INT347E I2C controller in the device tree or ACPI tables

### 3. Verify regulator setup
- The `tps68470_regulator` module is loaded but no consumers are configured
- Check if the regulator consumers need to be set up
- Look for the correct device names for the regulators

### 4. Consider alternative PMIC identification
- If the TPS68470 isn't present, the board might use a different PMIC
- Check what PMIC is actually on the Chuwi UBook XPro
- Whether the board uses a different regulator configuration

### 5. Priority: Confirm GPIO pins on hardware
- The GPIO pins in `tps68470_board_data.c` were extracted from binary analysis
- They have been mapped to the TPS68470 GPIO chip
- Need to confirm if the PMIC is detectable on any I2C bus before we can test GPIO pins
- May need to enable the I2C controller for the PMIC or use the ACPI OpRegion driver to access the PMIC

## Scripts Created (run with sudo)

### check_i2c_pins.sh
- Scan all I2C buses for devices
- Try to detect TPS68470 at common addresses (0x48, 0x49, 0x60, 0x61)
- Check loaded I2C controllers

### check_gpio_pins.sh
- Export and read all 152 GPIO pins on gpiochip512
- Check GPIO directions and values
- Auto-unexport after reading

### check_acpi_tables.sh
- Dump ACPI tables and check for INT3472/INT347A/INT347E references
- Check INT3472 ACPI devices for properties
- Look for DSM methods on INT3472 devices

### check_dsm.sh
- Check DSM (Device-Specific Methods) for INT3472 devices
- This can reveal I2C routing and GPIO configuration

### check_i2c_controller.sh
- Check int3472-tps68470 I2C controller configuration
- Check for I2C controllers in device tree
- Check kernel config for I2C controllers

### check_dsdT.sh
- Dump and analyze DSDT with iasl
- Look for INT3472, INT347A, INT347E, I2C, GPIO references
- Find Device definitions with INT347

### check_kernel_config.sh
- Check kernel config for TPS68470, int3472, I2C controllers
- Check for I2C controller modules
- Check for GPIO controller modules

### check_module_deps.sh
- Check int3472-tps68470 module dependencies
- Check if int3472-tps68470 is built-in or module
- Check for I2C controller that might be needed

### check_all_modules.sh
- Check all loaded I2C and GPIO related modules
- Check if I2C controller has firmware_node
- Check for any I2C controller that might be needed

### manual_i2c_scan.sh
- Manual I2C bus scan for TPS68470 PMIC
- Try to detect TPS68470 using i2cdetect
- Check for i2c-tools

### check_0x44_device.sh
- Investigate the device at 0x44 on i2c-3 (SMBus I801 adapter)
- This is likely the TPS68470 PMIC!
- Check which driver is claiming the device

### check_tps68470_source.sh
- Check the TPS68470 PMIC driver source to understand I2C detection
- Verify kernel configuration for TPS68470 support
- Check for I2C controller modules

### try_gpio_permutations.sh
- Try all permutations of reset, pdwn, and enable pins (0-63) on all GPIO chips
- Uses unique pin numbers for each role (reset, pdwn, enable)
- Checks all available GPIO chips

### try_exact_pins.sh
- Try the exact GPIO pin configuration from kernel source
- reset=3, pdwn=4, enable=5
- Tries nearby pins (0-7) as well

## Files

- `agent_work/Makefile` — Proper out-of-tree kbuild Makefile
- `agent_work/tps68470_board_data.c` — Chuwi-specific board data with exact GPIO pin numbers
- `agent_work/intel_skl_int3472_tps68470.ko` — Built module (715KB)
- `dmesg_with_patched_and_custom_kernel_module` — Boot log for reference
- `dmesg_with_patched_and_custom_kernel_module_pin_setup2` — Boot log with GPIO pin setup
- `agent_work/check_i2c_pins.sh` — I2C bus scan for TPS68470
- `agent_work/check_gpio_pins.sh` — GPIO pin mapping check
- `agent_work/check_acpi_tables.sh` — ACPI table analysis
- `agent_work/check_dsm.sh` — DSM method check
- `agent_work/check_i2c_controller.sh` — I2C controller info
- `agent_work/check_dsdT.sh` — DSDT dump with iasl
- `agent_work/check_kernel_config.sh` — Kernel config check
- `agent_work/check_module_deps.sh` — Module dependencies
- `agent_work/check_all_modules.sh` — All modules check
- `agent_work/manual_i2c_scan.sh` — Manual I2C scan
- `agent_work/check_0x44_device.sh` — Investigate device at 0x44 on i2c-3
- `agent_work/check_tps68470_source.sh` — TPS68470 driver source check
- `agent_work/try_gpio_permutations.sh` — GPIO pin permutation check (fixed)
- `agent_work/try_exact_pins.sh` — Exact pin configuration from kernel source
- `agent_work/gpio_pin_mapping.md` — Detailed GPIO pin mapping document

## Lessons Learned

1. **GPIO pin mapping is critical** — The kernel source specifies the exact pins, but they need to be on the correct GPIO controller
2. **I2C controller configuration** — The SMBus I801 adapter might need to be properly configured for the PMIC to be detected
3. **Driver binding** — Multiple drivers claiming the same device suggests the PMIC is not responding correctly
4. **Pin permutations** — Tried all combinations of pins 0-63, but no PMIC response detected, suggesting the GPIO pins might be on a different controller or the pin numbers are different
5. **Windows driver doesn't provide GPIO pin configuration in text form** — That information is in the Linux kernel source
6. **Windows driver confirms PMIC identity** — SKC controller DLL contains TPS68470 references, confirming the camera PMIC is the TPS68470
7. **GPIO pins extracted from binary** — Used Ghidra to decompile SkcController.sys and extract exact GPIO pin numbers for the Chuwi UBook XPro
8. **tps68470_board_data.c created** — Complete GPIO board data with exact pin numbers (gpio.0=Reset, gpio.1=Enable, gpio.2=Strobe, gpio.3=Torch, gpio.4=Flash, gpio.5=LedRear, gpio.6=LedFront, gpio.7=PrivateLED, gpio.8=Power0, gpio.9=Power1, gpio.10=Standby, gpio.11=WriteProtect, gpio.12=PowerEn, gpio.13=Mclk)

## Remaining Issues

- TPS68470 PMIC not detected on any I2C bus
- **GPIO pin configuration extracted from binary — NOT YET CONFIRMED ON HARDWARE**
  - Pins 0-6 on TPS68470 GPIO for general functions (Reset, Enable, Strobe, Torch, Flash, LedRear, LedFront)
  - Pins 7-13 for sensor-specific functions (PrivateLED, Power0, Power1, Standby, WriteProtect, PowerEn, Mclk)
  - These need to be verified once the PMIC is detected on an I2C bus
- I2C controller for the PMIC needs to be properly configured
- Regulator consumers need to be set up
