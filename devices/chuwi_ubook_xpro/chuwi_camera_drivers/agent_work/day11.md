# Day 11 — Handover: GPIO Pin Permutations Attempted, TPS68470 PMIC Not Detected

## Status

- **Module builds** — `make` in `agent_work/` succeeds cleanly
- **Module loads** — `lsmod` confirms `intel_skl_int3472_tps68470` loaded (20480 bytes)
- **Board data matched** — DMI match found `CHUWI UBook XPro`, ACPI device `INT3472:05` present
- **No dmesg errors** — Clean boot, no warnings related to the module
- **I2C PMIC not detected** — TPS68470 PMIC not found on any I2C bus (buses 4-8 empty)
- **GPIO pin permutations attempted** — Tried all combinations of pins 0-63 on all GPIO chips, no PMIC response detected
- **Regulator consumers not set up** — `tps68470_regulator` module loaded but no consumers configured

## What Worked

1. **Out-of-tree kbuild module** — Proper Makefile with `obj-m`, `xxx-y`, `KERNELRELEASE` pattern
2. **Source files copied** — `tps68470.c`, `tps68470.h`, `common.c`, `common.h`, `tps68470_board_data.c` all in `agent_work/`
3. **Module installs** — `make install` copies `.ko` and `.mod` to `/lib/modules/$(uname -r)/updates/`
4. **`sudo depmod -a`** — Module dependencies updated
5. **Kernel 6.8.0-137-generic** — Module loads with correct `vermagic`
6. **I2C bus scan** — Found device at 0x44 on i2c-3 (SMBus I801 adapter)
7. **Kernel source analysis** — Found exact GPIO pin configuration for Chuwi UBook XPro from kernel source

## What Needs Fixing

The Chuwi UBook XPro has ACPI devices `INT3472:00` through `INT3472:08`, but the TPS68470 PMIC isn't being detected on any I2C bus. The module matched the board data correctly, but the regulator setup requires the physical PMIC to be present on an I2C bus.

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

### GPIO Pin Configuration (from kernel source)
The kernel source specifies the exact GPIO pins for Chuwi UBook XPro:
- **INT347A GPIO 3**: reset (active low)
- **INT347A GPIO 4**: powerdown (active low)
- **INT347E GPIO 5**: enable (active high)

These are TPS68470 GPIO pins, not INT3472 GPIO pins.

### GPIO Controller Analysis
- Only one GPIO chip available: **gpiochip512** with 152 pins
- Tried all combinations of pins 0-63 on gpiochip512
- No PMIC response detected with any combination

### INT3472 ACPI Devices
- INT3472:00 through INT3472:08 exist but have no properties exposed
- No INT347A or INT347E devices found
- No DSM methods on any INT3472 devices

## Next Steps

### 1. Investigate the I2C controller for the PMIC
- The device at 0x44 on i2c-3 is likely the TPS68470 PMIC
- The I2C controller might need to be properly configured
- Check if the SMBus I801 adapter needs to be enabled in the kernel config
- Look for the INT347A/INT347E I2C controller in the device tree or ACPI tables

### 2. Check GPIO pin configuration
- The kernel source specifies pins 3, 4, 5 for reset, pdwn, enable
- These are TPS68470 GPIO pins, not INT3472 GPIO pins
- Check if the GPIO pins are correctly mapped in the ACPI tables
- Verify that the GPIO controller is properly configured

### 3. Verify regulator setup
- The `tps68470_regulator` module is loaded but no consumers are configured
- Check if the regulator consumers need to be set up
- Look for the correct device names for the regulators

### 4. Consider alternative PMIC identification
- If the TPS68470 isn't present, the board might use a different PMIC
- Check what PMIC is actually on the Chuwi UBook XPro
- Whether the board uses a different regulator configuration

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

## Commands to Run

```bash
# Check I2C buses
for i in 0 1 2 3 4 5 6 7 8; do echo "=== i2c-$i ==="; ls /sys/bus/i2c/devices/i2c-$i/ 2>/dev/null; done

# Check ACPI devices
ls /sys/bus/acpi/devices/ | grep INT3472

# Check I2C addresses
for addr in 0x48 0x49 0x60 0x61; do echo "=== Scanning $addr ==="; i2cdetect -y 3 2>/dev/null | grep $addr; done

# Check GPIO mapping
cat /proc/gpio 2>/dev/null | grep -i "tps68470\|INT347"

# Run the scripts (all require sudo)
sudo bash agent_work/check_i2c_pins.sh
sudo bash agent_work/check_gpio_pins.sh
sudo bash agent_work/check_acpi_tables.sh
sudo bash agent_work/check_dsm.sh
sudo bash agent_work/check_i2c_controller.sh
sudo bash agent_work/check_dsdT.sh
sudo bash agent_work/check_kernel_config.sh
sudo bash agent_work/check_module_deps.sh
sudo bash agent_work/check_all_modules.sh
sudo bash agent_work/manual_i2c_scan.sh
sudo bash agent_work/check_0x44_device.sh
sudo bash agent_work/check_tps68470_source.sh
sudo bash agent_work/try_gpio_permutations.sh
sudo bash agent_work/try_exact_pins.sh
```

## Files

- `agent_work/Makefile` — Proper out-of-tree kbuild Makefile
- `agent_work/tps68470_board_data.c` — Chuwi-specific board data
- `agent_work/intel_skl_int3472_tps68470.ko` — Built module (715KB)
- `dmesg_with_patched_and_custom_kernel_module` — Boot log for reference
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

## Lessons Learned

1. **GPIO pin mapping is critical** — The kernel source specifies the exact pins, but they need to be on the correct GPIO controller
2. **I2C controller configuration** — The SMBus I801 adapter might need to be properly configured for the PMIC to be detected
3. **Driver binding** — Multiple drivers claiming the same device suggests the PMIC is not responding correctly
4. **Pin permutations** — Tried all combinations of pins 0-63, but no PMIC response detected, suggesting the GPIO pins might be on a different controller or the pin numbers are different

## Remaining Issues

- TPS68470 PMIC not detected on any I2C bus
- GPIO pin configuration needs to be verified
- I2C controller for the PMIC needs to be properly configured
- Regulator consumers need to be set up
