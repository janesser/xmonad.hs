# Handoff: Day 7 - INT3472 ACPI Patching Progress

## 🎯 Goal Status
The ACPI patching approach is **partially working** but not yet complete. The INT3472/TPS68470 driver is loaded but not creating regulators or GPIO devices.

## ✅ Accomplishments (Today)
- **INT3471:00 "hash matches"** - ACPI patching is working for at least one INT3471 device
- **INT3472/TPS68470 modules loaded**:
  - `tps68470_regulator` (12288 bytes)
  - `clk_tps68470` (12288 bytes)
  - `intel_skl_int3472_tps68470` (20480 bytes)
  - `intel_skl_int3472_discrete` (24576 bytes)
- **ACPI patch compiled successfully**: `dsdt.patched.aml` (182KB)
- **CPIO override created**: `/boot/acpi_override_patched.cpio`
- **GRUB menu entry added**: "Ubuntu (patched DSDT)" in `/etc/grub.d/40_custom`

## 🚨 Critical Issue: No Regulators Created
**INT3472/TPS68470 modules are loaded but NO regulators or GPIO devices are being created.**

The INT3472 driver needs to:
1. Find INT3472 ACPI devices in the DSDT
2. Call CLDB methods to create GPIO platform devices
3. Create regulator nodes (DOVDD, DVDD, AVDD)
4. OV2680 driver can then find regulators and produce video

**Current state**: INT3472 devices exist in ACPI tables but driver isn't creating regulators.

## 🔍 Next Steps for Tomorrow

### Priority 1: Diagnose Why INT3472 Driver Isn't Creating Regulators
1. Check if INT3472 devices are being found by the driver
2. Verify CLDB methods are being called
3. Check if GPIO pin mappings are correct for Chuwi Ubook XPro
4. Compare with Latitude 5290 working configuration

### Priority 2: Fix INT3472 CLDB Methods
The ACPI patch set `control_logic_type=0x02` in CLDB methods, but:
- May need to update GPIO pin numbers for Chuwi Ubook XPro
- May need to add additional methods for regulator control
- May need to fix device HID matching

### Priority 3: Test OV2680 Driver
Once regulators are created:
- Check if OV2680 deferred probe resolves
- Verify /dev/video* devices appear
- Test camera functionality

## 📋 Hardware Details
- **Sensors**: OV2680 (CAM0, I2C2), OV5648 (CAM1, I2C2)
- **PMIC**: INT3472 with TPS68470 regulators
- **ACPI CLDB (current)**: control_logic_type=0x02 (PMIC TPS68470) ← patched
- **INT3472 devices in ACPI**: Multiple instances (UID 0, 1, ...) in DSDT

## 📁 Key Files
- `dsdt.patched.dsl` - Modified DSDT with CLDB methods updated
- `dsdt.patched.aml` - Compiled binary ACPI table (182KB)
- `install-acpi-patch.sh` - Installation script
- `ACPI-PATCH-README.md` - Step-by-step guide
- `/tmp/pi-github-repos/jelsco/latitude-5290-camera/` - Reference repo

## 🔧 Tools Needed
- `acpica-tools` (iasl, acpidump)
- `dkms` (for building kernel modules)
- Kernel source (6.8.0-136-generic modules)

## 📊 Current System State
```
Loaded modules: ov2680, mxc4005, int3472-tps68470, tps68470_regulator, clk_tps68470
INT3471:00: hash matches (ACPI patch working)
INT3472/TPS68470: modules loaded but no devices bound
No /dev/video* devices present
No regulators exposed (only regulator-dummy)
ACPI CLDB control_logic_type = 0x02 (patched) ← but driver not creating regulators
```

## 📝 Notes for Next Agent
- The ACPI patch changed CLDB methods to set control_logic_type=0x02
- INT3471:00 "hash matches" confirms patch is being loaded
- INT3472/TPS68470 modules loaded but not creating devices
- Need to investigate why INT3472 driver isn't finding GPIOs/regulators
- May need to check GPIO pin mappings for Chuwi Ubook XPro (different from Dell Latitude 5290)
- Reference: Latitude 5290 uses GPIO pin 3 (reset, active-low) and GPIO pin 4 (powerdown, active-low)
