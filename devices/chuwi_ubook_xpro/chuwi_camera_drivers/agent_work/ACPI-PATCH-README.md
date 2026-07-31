# ACPI Patch for Chuwi Ubook XPro INT3472 TPS68470 Board Data

This patch modifies the ACPI tables to set control_logic_type=0x02 (PMIC TPS68470) instead of 0x00 (UNKNOWN) for all INT3472 devices. This causes the INT3472 TPS68470 driver to create regulators (DOVDD, DVDD, AVDD) instead of going to the ChromeOS path (which doesn't create regulators).

## What This Patch Does

Changes the CLDB (Camera Local Buffer Data) method in the INT3472 ACPI devices to return control_logic_type=0x02 instead of reading from the C0TP/C1TP/C2TP/C3TP fields (which are currently 0x00).

## Files

- `dsdt.patched.dsl` - Modified DSDT with CLDB methods updated
- `dsdt.patched.aml` - Compiled binary ACPI table (ready to use)

## Prerequisites

- `acpica-tools` package (provides `iasl` compiler)
- GRUB with initrd support
- Root access

## Installation (GRUB with initrd)

### Step 1: Compile the patched ACPI table

```bash
iasl -sa dsdt.patched.dsl
```

This creates `dsdt.patched.aml` (already done above).

### Step 2: Create initrd cpio

```bash
mkdir -p /tmp/patched-acpi
cp dsdt.patched.aml /tmp/patched-acpi/dsdt.aml
mkdir -p kernel/firmware/acpi
cp /tmp/patched-acpi/dsdt.aml kernel/firmware/acpi/dsdt.aml
find kernel -print0 | cpio -H newc --create -0 > /tmp/patched_acpi_tables.cpio
```

### Step 3: Install initrd for GRUB

```bash
# Copy initrd to EFI partition
sudo mkdir -p /boot/efi/EFI/acpi
sudo cp /tmp/patched_acpi_tables.cpio /boot/efi/EFI/acpi/

# GRUB will automatically detect and load the initrd
# The kernel will use the patched ACPI tables on next boot
```

### Step 4: Reboot and verify

```bash
sudo reboot
```

After reboot, check:
```bash
dmesg | grep -iE "TPS68470|regulator|INT3472"
ls /sys/class/regulator/ | grep -iE "dovdd|dvdd|avdd"
```

You should see regulators registered and the OV2680 sensor should produce video.

## Reversing the Patch

To revert to the original ACPI tables:

```bash
# Remove the patched initrd
sudo rm /boot/efi/EFI/acpi/patched_acpi_tables.cpio

# Reboot with original tables
sudo reboot
```

Or, if you want to keep the original DSDT and apply the patch selectively:

```bash
# Keep original dsdt.dsl and dsdt.patched.aml
# Use the original dsdt.aml in initrd
```

## Alternative: Use systemd-boot

If you're using systemd-boot instead of GRUB:

```bash
# Copy to EFI partition
sudo mkdir -p /boot/efi/EFI/acpi
sudo cp /tmp/patched_acpi_tables.cpio /boot/efi/EFI/acpi/

# Edit boot entry
sudo vim /boot/efi/loader/entries/Pop_OS-current.conf
# Add: initrd /EFI/acpi/patched_acpi_tables.cpio
```

## Safety Notes

- **This is reversible** - just remove the initrd and reboot
- **Keep the original dsdt.dsl** as backup
- **Test on a non-production system first**
- **The patch modifies the DSDT** - this is the main ACPI table, so be cautious
- **If something goes wrong**, boot from a USB stick with the original tables

## Troubleshooting

### Regulators still not appearing

1. Check that the INT3472 driver is loaded:
   ```bash
   lsmod | grep int3472
   ```

2. Check for INT3472 devices:
   ```bash
   ls /sys/bus/i2c/devices/ | grep -i "INT347\|tps68470"
   ```

3. Check dmesg for errors:
   ```bash
   dmesg | grep -i "int3472\|tps68470"
   ```

### Camera still not working

1. Check I2C devices:
   ```bash
   ls /sys/class/i2c-dev/ | grep -i "OVTI\|OV56"
   ```

2. Check video devices:
   ```bash
   ls /dev/video*
   ```

3. Check media topology:
   ```bash
   media-ctl -d /dev/media0 info
   ```

## Reference

- Latitude 5290 approach: https://github.com/jelsco/latitude-5290-camera
- ACPI patching guide: https://gist.github.com/lamperez/d5b385bc0c0c04928211e297a69f32d7
- Kernel driver: https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/platform/x86/intel/int3472/tps68470.c
