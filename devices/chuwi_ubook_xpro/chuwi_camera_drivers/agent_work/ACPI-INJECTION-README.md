# ACPI Table Injection for GRUB 2

## Overview

This setup allows you to boot Linux with different ACPI DSDT tables:
- **Stock BIOS** - original DSDT from BIOS (for comparison/rollback)
- **acpidump DSDT** - DSDT extracted from BIOS using acpidump
- **Patched DSDT** - custom-patched DSDT (from agent_work/dsdt.patched.dsl)

## Files

- `acpi-inject.sh` - Main setup script (converts .dsl → .aml, creates CPIO archives, configures GRUB)
- `ACPI-INJECTION-README.md` - This file

## Requirements

- `iasl` (ACPI compiler) - install with: `sudo apt install acpica-tools`
- Root/sudo access to modify `/etc/default/grub` and `/etc/grub.d/`

## Quick Start

```bash
# 1. Run the setup script
sudo /home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers/acpi-inject.sh

# 2. Reboot and test
sudo reboot

# 3. At GRUB menu, try each option:
#    - Ubuntu (stock BIOS) - original behavior
#    - Ubuntu (acpidump DSDT) - extracted from BIOS
#    - Ubuntu (patched DSDT) - your custom patch
```

## How It Works

1. **Convert .dsl → .aml**: `iasl -d` converts ASL source to AML binary
2. **Create CPIO archives**: Each DSDT version is packed into a CPIO file
3. **GRUB loads CPIO as initrd**: Using `GRUB_EARLY_INITRD_LINUX_CUSTOM`, GRUB loads the CPIO before the kernel
4. **Kernel uses custom ACPI tables**: The kernel reads ACPI tables from the CPIO initrd instead of BIOS

## Reverting

To revert to stock BIOS:
```bash
# 1. Edit /etc/default/grub and remove/change:
#    GRUB_EARLY_INITRD_LINUX_CUSTOM="acpi_override_acpidump"
#    To: GRUB_EARLY_INITRD_LINUX_CUSTOM="" (empty)

# 2. Rebuild GRUB
sudo update-grub

# 3. Reboot
sudo reboot
```

Or use the "Ubuntu (stock BIOS)" menu entry which doesn't load any CPIO initrd.

## Troubleshooting

### GRUB doesn't show the new entries
- Check `/etc/grub.d/40_custom` exists and has content
- Run `sudo update-grub` to rebuild
- Check for syntax errors in the GRUB config

### Kernel doesn't boot with CPIO initrd
- The CPIO file might be corrupt - recreate it
- Check that `iasl` successfully compiled the `.aml` file
- Verify the CPIO file is valid: `file /boot/acpi_override_*.cpio`

### ACPI tables not overriding
- Check dmesg for ACPI-related errors
- The kernel might need `CONFIG_ACPI_TABLE_UPGRADE` enabled
- Try different kernel versions (some have better ACPI table override support)

### CPIO file not found
- Verify the CPIO files exist in `/boot/`
- Check permissions on `/boot/`
- Re-run `acpi-inject.sh`

## Advanced: Creating Custom Patches

To create a new patched DSDT:
1. Extract DSDT from BIOS: `acpidump -b -n DSDT > dsdt.dsl`
2. Edit `dsdt.dsl` with your changes
3. Convert to AML: `iasl -d dsdt.dsl -o dsdt.aml`
4. Place `.dsl` and `.aml` in a directory (e.g., `agent_work/`)
5. Run `acpi-inject.sh` to set up GRUB

## Kernel Requirements

The kernel must support ACPI table override via initrd. Most modern kernels support this, but if you have issues:
- Check `CONFIG_ACPI_TABLE_UPGRADE` in kernel config
- Try mainline kernels (they have better ACPI support)
- Some kernels require `ACPI_TABLE_OVERRIDE_VIA_BUILTIN_INITRD`

## References

- [Linux Kernel Documentation: ACPI Table Override via initrd](https://docs.kernel.org/7.0/admin-guide/acpi/initrd_table_override.html)
- [Arch Wiki: DSDT](https://wiki.archlinux.org/title/DSDT)
- [AskUbuntu: Custom ACPI DSDT with GRUB2](https://askubuntu.com/questions/1523661/how-to-correctly-use-a-custom-acpi-dsdt-table-using-grub2-ubuntu-24-04)
