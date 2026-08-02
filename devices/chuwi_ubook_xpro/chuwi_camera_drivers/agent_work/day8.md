# Handoff: Day 8 - INT3472/TPS68470 Driver Root Cause Analysis

## 🎯 Goal Status
The ACPI patching approach is partially working but the INT3472/TPS68470 driver is not creating regulators or GPIO devices. Root cause identified: board data lookup fails due to DMI mismatch.

## ✅ Accomplishments (Today)
- **Kernel source extracted**: Downloaded and extracted linux-source-6.8.0.tar.bz2 to project directory
- **INT3472 driver source analyzed**: Read full INT3472/TPS68470 driver code (tps68470.c, tps68470_board_data.c, common.c, discrete.c)
- **Root cause identified**: `int3472_tps68470_get_board_data()` returns NULL because DMI system vendor is not "Microsoft Corporation" and product name is not "Surface Go"
- **CLDB structure understood**: 
  - Byte 0: version
  - Byte 1: control_logic_type (0x02 = Windows/TPS68470)
  - Bytes 2-3: control_logic_id, sensor_card_sku
  - Bytes 4-13: reserved
  - Byte 14: clock_source
  - Bytes 15-31: reserved2
  - GPIO pins at offsets 0x09-0x0D (C0W1-C0W5 for DSC0)
- **ACPI patch status**: Current boot is NOT using patched ACPI table (booting default menu entry, not "Ubuntu (patched DSDT)")
- **Windows drivers inaccessible**: /home/jan/Downloads/UBook XPro/Drivers/ has filesystem access issues despite find traversing it
- **Handover file created**: agent_work/day8.md with detailed analysis

## 🚨 Critical Issue: Board Data Lookup Fails
**The INT3472/TPS68470 driver flow:**
1. `skl_int3472_tps68470_probe()` matches ACPI device with HID "INT3472"
2. Gets ACPI companion device
3. Calls `skl_int3472_fill_clk_pdata()` to get sensor consumer info
4. Initializes regmap for TPS68470 I2C communication
5. Calls `tps68470_chip_init()` to force software reset and read revision
6. Calls `skl_int3472_tps68470_calc_type()` to check CLDB for control_logic_type
7. If control_logic_type == 2 (Windows), calls `int3472_tps68470_get_board_data(dev_name)`
8. **If board_data is NULL, returns error: "No board-data found for this model"**
9. No MFD cells created (no GPIO, Clock, or Regulator platform devices)
10. tps68470-regulator driver has nothing to bind to → no regulators exposed

**Board data lookup mechanism:**
- Uses DMI system info (vendor, product name) to find matching board data
- Board data contains:
  - `tps68470_regulator_pdata`: Regulator configuration (voltages, consumer supplies)
  - `tps68470_gpio_lookup_tables`: GPIO pin mappings (e.g., GPIO 9=reset, GPIO 7=powerdown)
  - `dev_name`: Device name to match (e.g., "i2c-INT3472:05")

**Current board data (Surface Go):**
```c
static struct gpiod_lookup_table surface_go_int347a_gpios = {
    .dev_id = "i2c-INT347A:00",
    .table = {
        GPIO_LOOKUP("tps68470-gpio", 9, "reset", GPIO_ACTIVE_LOW),
        GPIO_LOOKUP("tps68470-gpio", 7, "powerdown", GPIO_ACTIVE_LOW),
        { }
    }
};

static struct gpiod_lookup_table surface_go_int347e_gpios = {
    .dev_id = "i2c-INT347E:00",
    .table = {
        GPIO_LOOKUP("tps68470-gpio", 5, "enable", GPIO_ACTIVE_HIGH),
        { }
    }
};
```

## 🔍 Key Files Analyzed
- `linux-source-6.8.0/drivers/platform/x86/intel/int3472/tps68470.c` - Main driver
- `linux-source-6.8.0/drivers/platform/x86/intel/int3472/tps68470_board_data.c` - Board data with DMI lookup
- `linux-source-6.8.0/drivers/platform/x86/intel/int3472/common.c` - CLDB filling
- `linux-source-6.8.0/drivers/platform/x86/intel/int3472/common.h` - Data structures
- `linux-source-6.8.0/drivers/platform/x86/intel/int3472/discrete.c` - Discrete camera driver
- `linux-source-6.8.0/drivers/regulator/tps68470-regulator.c` - Regulator driver
- `linux-source-6.8.0/include/linux/platform_data/tps68470.h` - Platform data header
- ACPI dump: `acpidump/dsdt.dsl` - Shows INT3472 devices (DSC0, DSC1, DSC2, DSC3)

## 📊 Current System State
```
Loaded modules: ov2680, mxc4005, int3472-tps68470, tps68470_regulator, clk_tps68470
I2C devices: i2c-OVTI2680:00 (I2C2), i2c-OVTI5648:00 (I2C2), i2c-MXC6655:00 (I2C0)
No /dev/video* devices present
No regulators exposed (only regulator-dummy from ov2680 fallback)
INT3472/TPS68470 driver loaded but not matching any ACPI device or not creating MFD cells
ACPI patch not currently applied (booting default menu entry, not "Ubuntu (patched DSDT)")
```

## 📝 Notes for Next Agent
- The INT3472/TPS68470 driver needs to be modified to work without DMI board data lookup
- **Need to determine correct GPIO pin numbers for Chuwi Ubook XPro** (Surface Go uses 9, 7, 5)
- ACPI patch sets control_logic_type=0x02 in CLDB methods, which tells driver this is Windows/TPS68470
- Windows drivers are in /home/jan/Downloads/UBook XPro/Drivers/ but have filesystem access issues
- Kernel source is available at linux-source-6.8.0/
- Previous handoff at day8.md has detailed analysis
