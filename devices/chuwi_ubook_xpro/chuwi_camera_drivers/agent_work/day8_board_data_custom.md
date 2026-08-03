# Handoff: Day 8 - Custom INT3472/TPS68470 Board Data for Chuwi UBook XPro

## 🎯 Goal
Create a custom INT3472/TPS68470 kernel module with board data for Chuwi UBook XPro to bypass the DMI lookup failure that prevents regulators from being created.

## ✅ Accomplishments (Today)

### 1. Root Cause Analysis
- **Identified the problem**: INT3472/TPS68470 driver's `int3472_tps68470_get_board_data()` uses DMI system info to find GPIO pin mappings
- **DMI mismatch**: Chuwi UBook XPro DMI vendor is "CHUWI Innovation And Technology(ShenZhen)co.,Ltd" but board data table only has "Microsoft Corporation" / "Surface Go" entries
- **Driver flow**: 
  1. `skl_int3472_tps68470_probe()` matches ACPI device with HID "INT3472"
  2. Checks CLDB buffer for `control_logic_type` (needs 0x02 for Windows/TPS68470)
  3. Calls `int3472_tps68470_get_board_data(dev_name)` to get GPIO pin mappings
  4. If board_data is NULL, returns error: "No board-data found for this model"
  5. No MFD cells created → no regulators, no GPIOs

### 2. Kernel Source Analysis
- **Extracted kernel source**: Downloaded and extracted linux-source-6.8.0.tar.bz2
- **Analyzed key files**:
  - `drivers/platform/x86/intel/int3472/tps68470.c` - Main driver with probe function
  - `drivers/platform/x86/intel/int3472/tps68470_board_data.c` - Board data with DMI lookup table
  - `drivers/platform/x86/intel/int3472/common.c` - CLDB buffer filling
  - `drivers/platform/x86/intel/int3472/common.h` - Data structures
  - `drivers/regulator/tps68470-regulator.c` - Regulator driver
  - `include/linux/platform_data/tps68470.h` - Platform data header

### 3. CLDB Structure Understanding
- **Buffer layout** (32 bytes):
  - Byte 0: version
  - Byte 1: control_logic_type (0x00=ChromeOS, 0x02=Windows/TPS68470)
  - Byte 2: control_logic_id
  - Byte 3: sensor_card_sku
  - Bytes 4-13: reserved
  - Byte 14: clock_source
  - Bytes 15-31: reserved2
- **GPIO pins** at offsets 0x09-0x0D in CLDB buffer (C0W1-C0W5 for DSC0)

### 4. Board Data Modification
- **Created modified `tps68470_board_data.c`** in agent_work/ with:
  - Added Chuwi UBook XPro board data entry with Surface Go GPIO pins as starting point
  - DMI match: "CHUWI Innovation And Technology(ShenZhen)co.,Ltd" / "UBook XPro"
  - GPIO pins (using Surface Go values):
    - INT347A:00 (CAM0): GPIO 9 (reset, active-low), GPIO 7 (powerdown, active-low)
    - INT347E:00 (CAM1): GPIO 5 (enable, active-high)
  - Same regulator voltages as Surface Go (CORE=1.2V, ANA=2.8152V, VCM=2.8152V, VIO=1.8006V, VSIO=1.8006V, AUX1=2.8152V, AUX2=1.8006V)

### 5. Build Attempts
- **Attempt 1**: Built in project directory kernel source - failed due to missing Module.symvers
- **Attempt 2**: Fixed Module.symvers by copying from kernel build directory - module.o created but .ko file not created
- **Attempt 3**: Tried building with kernel headers - no permission to write to kernel headers directory
- **Current status**: Module.o created successfully but need .ko file creation step

## 🚨 Critical Issues

### 1. Build System Issue
- **Problem**: `make M=...` creates .o file but .ko file creation fails
- **Error**: MODPOST creates .mod file but .ko file rule not found
- **Likely cause**: Missing `scripts/external/kmod` or incorrect make target for .ko creation
- **Need**: Either fix build system or find alternative way to create .ko from .o

### 2. Kernel Build Directory Access
- **Problem**: No sudo access to copy files to kernel build directory
- **Impact**: Cannot build modules using standard `make -C /lib/modules/.../build` approach
- **Workaround**: Need to build in project directory and handle .ko creation

### 3. Windows Drivers Inaccessible
- **Problem**: /home/jan/Downloads/UBook XPro/Drivers/ has filesystem access issues
- **Impact**: Cannot examine Windows driver for correct GPIO pin numbers
- **Impact**: Using Surface Go GPIO pins (9, 7, 5) as starting point may be incorrect

## 📊 Current System State
```
Loaded modules: ov2680, mxc4005, int3472-tps68470, tps68470_regulator, clk_tps68470
I2C devices: i2c-OVTI2680:00 (I2C2), i2c-OVTI5648:00 (I2C2), i2c-MXC6655:00 (I2C0)
No /dev/video* devices present
No regulators exposed (only regulator-dummy from ov2680 fallback)
INT3472/TPS68470 driver loaded but not matching any ACPI device or not creating MFD cells
ACPI patch not currently applied (booting default menu entry, not "Ubuntu (patched DSDT)")
```

## 📁 Key Files
- `agent_work/tps68470_board_data.c` - Modified board data with Chuwi DMI entry
- `agent_work/compile_intel_skl_int3472_chuwi_custom.sh` - Build script (needs sudo)
- `linux-source-6.8.0/` - Kernel source tree (needs .ko file creation fix)
- `acpidump/dsdt.dsl` - ACPI table showing INT3472 devices

## 🔧 Build Script

```bash
#!/bin/bash
# compile_intel_skl_int3472_chuwi_custom.sh

PROJECT_DIR="/home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers"
AGENT_WORK="${PROJECT_DIR}/agent_work"
KERNEL_BUILD="/lib/modules/6.8.0-136-generic/build"
BUILD_DIR="${KERNEL_BUILD}/drivers/platform/x86/intel/int3472"
OUTPUT_KO="${AGENT_WORK}/intel_skl_int3472_tps68470_custom.ko"

# Step 1: Copy modified source to kernel build directory
sudo cp "${AGENT_WORK}/tps68470_board_data.c" "${BUILD_DIR}/tps68470_board_data.c"

# Step 2: Build the module
sudo make -C "${KERNEL_BUILD}" M="${BUILD_DIR}" intel_skl_int3472_tps68470.ko 2>&1 | tail -20

# Step 3: Copy the built module
sudo cp "${BUILD_DIR}/intel_skl_int3472_tps68470.ko" "${OUTPUT_KO}"
```

## 📝 Next Steps

### Option A: Fix .ko File Creation
1. Investigate why .ko file is not being created from .o file
2. Check if `scripts/external/kmod` is available or needs to be created
3. Try alternative make targets or manual .ko creation using objcopy

### Option B: Alternative Build Approach
1. Use `dkms` if available (not installed)
2. Use `make modules` with the kernel build directory
3. Copy the .o file and manually create .ko using kernel tools

### Option C: Verify GPIO Pin Numbers
1. Examine Windows driver INF files for correct GPIO pin mappings
2. Check if there's a way to access /home/jan/Downloads/UBook XPro/Drivers/
3. Compare with Surface Go known GPIO pins (9, 7, 5)

### Option D: Test with Current Module
1. Load the existing int3472-tps68470 module (already loaded)
2. Check if any regulators appear after reboot with patched ACPI
3. Verify if the ACPI patch is actually being applied

## 📝 Notes for Next Agent
- The modified `tps68470_board_data.c` is in agent_work/ and includes Chuwi DMI entry
- Need to create a valid .ko file to load with `insmod`
- Surface Go GPIO pins (9, 7, 5) used as starting point - may need adjustment
- Windows drivers location: /home/jan/Downloads/UBook XPro/Drivers/ (access issues)
- Kernel source: linux-source-6.8.0/ in project directory
- ACPI patch file: /boot/acpi_override_patched.cpio
- GRUB menu entry "Ubuntu (patched DSDT)" exists but not currently selected
