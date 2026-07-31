# Handoff: Day 7 - Camera Driver Progress

## 🎯 Goal Status
The Chuwi Ubook XPro camera driver investigation has reached a critical inflection point. The Latitude 5290 repo (https://github.com/jelsco/latitude-5290-camera) provides a proven approach for fixing the INT3472 TPS68470 board-data issue, and the ACPI patching approach via `iasl` is reversible and well-documented.

## ✅ Accomplishments (Today)
- **Latitude 5290 Approach Identified:** The reference repo shows exactly how to solve the INT3472/TPS68470 board-data mismatch
- **ACPI Patching Approach Confirmed:** The gist (https://gist.github.com/lamperez/d5b385bc0c0c04928211e297a69f32d7) shows how to patch ACPI tables to set control_logic_type=0x02 (PMIC TPS68470) instead of 0x00 (UNKNOWN)
- **Key Insight:** Our ACPI CLDB buffer has control_logic_type=0x00, causing the INT3472 driver to go ChromeOS path (no regulators). Changing to 0x02 would trigger Windows path (creates regulators)
- **Reversibility:** The ACPI patch is fully reversible (can restore original CLDB buffer)
- **ACPI Patch Created:** Modified `dsdt.patched.dsl` with all 8 INT3472 CLDB methods updated to set control_logic_type=0x02
- **Compiled Successfully:** `dsdt.patched.aml` (182KB) compiled with `iasl -sa`
- **Installation Script Created:** `install-acpi-patch.sh` automates the GRUB initrd installation
- **Documentation Created:** `ACPI-PATCH-README.md` with step-by-step installation guide

## 🚀 Recommended Next Actions

### Option A: Patch ACPI Tables (Recommended)
**Approach:** Modify the CLDB buffer in our ACPI tables to set control_logic_type=0x02

**Steps:**
1. Install `acpica-tools` (`sudo apt install acpica-tools`)
2. Extract ACPI tables: `sudo acpidump -b`
3. Disassemble: `iasl -d dsdt.dat` (or the specific table containing INT3472)
4. Modify the CLDB buffer:
   - Change `PAR [Zero] = 0x00` to `PAR [Zero] = 0x02` (control_logic_type)
   - This is in the INT3472 device's CLDB method
5. Assemble: `iasl -sa dsdt.dsl`
6. Generate cpio: `find kernel | cpio -H newc --create > patched_acpi_tables.cpio`
7. Install: `sudo cp patched_acpi_tables.cpio /boot/efi/EFI/acpi/`
8. Reboot and verify: regulators should appear

**Reversibility:** Keep the original dsdt.dsl, can restore anytime

**Expected Result:** INT3472 driver matches our device, creates regulators (DOVDD, DVDD, AVDD), OV2680 sensor can power up

### Option B: Create Custom Board Data (Alternative)
**Approach:** Create a custom board data structure for our hardware and add a fallback path

**Pros:**
- No ACPI modification needed
- More "proper" approach

**Cons:**
- Need to determine correct GPIO mappings for our hardware
- Need to modify the INT3472 driver source
- More complex implementation

**Required Info:**
- Which GPIOs on TPS68470 control regulators (reset, powerdown, etc.)
- Regulator voltages and sequences

**Note:** The Latitude 5290 reuses `dell_7212_tps68470_board_data` which has GPIO pin 3 (reset, active-low) and GPIO pin 4 (powerdown, active-low). Our hardware may have different GPIO mappings.

### Option C: Standalone Platform Driver (Alternative)
**Approach:** Create a standalone platform driver for TPS68470 that bypasses INT3472 ACPI driver

**Pros:**
- Complete control over hardware configuration
- No dependency on INT3472 ACPI match

**Cons:**
- Need to implement full PMIC register access
- Need to determine correct GPIO mappings
- More code to maintain

## 📋 Hardware Details (Confirmed)
- **Sensors:** OV2680 (CAM0, I2C2), OV5648 (CAM1, I2C2)
- **PMIC:** INT3472 with TPS68470 regulators
- **ACPI CLDB (current):** control_logic_type=0x00 (UNKNOWN) → ChromeOS path (no regulators)
- **ACPI CLDB (desired):** control_logic_type=0x02 (PMIC TPS68470) → Windows path (creates regulators)
- **INT3472 devices in ACPI:** Multiple instances (UID 0, 1, ...) in DSDT

## 📁 Key Files
- `agent_work/day6.md` - Previous handoff
- `dsdt.cam0_cam1.dsl` - DSDT with INT3471/INT3474 ACPI devices
- `acpidump/dsdt.dsl` - Full DSDT dump (contains INT3472 CLDB methods)
- `dsdt.patched.dsl` - Modified DSDT with CLDB methods updated (8 INT3472 devices)
- `dsdt.patched.aml` - Compiled binary ACPI table (182KB, ready to use)
- `install-acpi-patch.sh` - Installation script for GRUB initrd
- `ACPI-PATCH-README.md` - Step-by-step installation guide
- `/tmp/pi-github-repos/jelsco/latitude-5290-camera/` - Latitude 5290 reference repo
  - `patches/0001-platform-x86-int3472-Add-Dell-Latitude-5290-2-in-1-D.patch` - DMI match approach
  - `kernel/int3472_tps68470/tps68470.c` - INT3472 driver source
  - `kernel/int3472_tps68470/tps68470_board_data.c` - Board data table
- https://gist.github.com/lamperez/d5b385bc0c0c04928211e297a69f32d7 - ACPI patching guide

## 🔧 Tools Needed
- `acpica-tools` (iasl, acpidump)
- `dkms` (for building kernel modules)
- Kernel source (6.8.0-136-generic modules available)

## 📊 Current System State
```
Loaded modules: ov2680, mxc4005, int3472-tps68470, tps68470_regulator, clk_tps68470
I2C devices: i2c-OVTI2680:00 (I2C2), i2c-OVTI5648:00 (I2C2), i2c-MXC6655:00 (I2C0)
No /dev/video* devices present
No regulators exposed (only regulator-dummy)
INT3472/TPS68470 driver loaded but no devices bound
ACPI CLDB control_logic_type = 0x00 (needs to be 0x02)
```

## 📝 Notes for Next Agent
- The Latitude 5290 approach adds a DMI match for "Latitude 5290 2-in-1" → `dell_7212_tps68470_board_data`
- Our hardware is Chuwi Ubook XPro (not Dell), so DMI match won't work
- ACPI patching (Option A) is the simplest and most direct approach
- The CLDB buffer is in the INT3472 device's method in the DSDT
- After patching, the INT3472 driver should create regulators and GPIO platform devices
- Then OV2680 driver should be able to find regulators and produce video
