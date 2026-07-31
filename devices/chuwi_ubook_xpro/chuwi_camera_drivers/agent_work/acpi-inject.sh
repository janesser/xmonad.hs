#!/bin/bash
# ACPI Table Injection Script for GRUB 2
# Converts .dsl files to .aml and creates CPIO archives for initrd override

set -e

# Paths
DSLT_DIR="/home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/acpidump"
PATCHED_DIR="/home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers/agent_work"
BOOT_DIR="/boot"
IASL="/usr/local/bin/iasl"

# Check for iasl
if [ ! -f "$IASL" ]; then
    echo "Error: iasl not found at $IASL"
    echo "Install with: sudo apt install acpica-tools"
    exit 1
fi

# Check for input files
if [ ! -f "$DSLT_DIR/dsdt.dsl" ]; then
    echo "Error: $DSLT_DIR/dsdt.dsl not found"
    exit 1
fi

if [ ! -f "$PATCHED_DIR/dsdt.patched.dsl" ]; then
    echo "Error: $PATCHED_DIR/dsdt.patched.dsl not found"
    exit 1
fi

echo "=== ACPI Table Injection Setup ==="

# Step 1: Convert .dsl → .aml
echo "Converting .dsl files to .aml..."

iasl -d -o "$BOOT_DIR/dsdt_acpidump.aml" "$DSLT_DIR/dsdt.dsl"
iasl -d -o "$BOOT_DIR/dsdt_patched.aml" "$PATCHED_DIR/dsdt.patched.dsl"

echo "Created AML files in $BOOT_DIR/"

# Step 2: Create CPIO archives
echo "Creating CPIO archives..."

# Original (stock BIOS) DSDT - create from acpidump if available
if [ -f "$DSLT_DIR/dsdt.dat" ]; then
    mkdir -p "$BOOT_DIR/acpi_override"
    cp "$DSLT_DIR/dsdt.dat" "$BOOT_DIR/acpi_override/dsdt.dat"
    cp "$DSLT_DIR/dsdt.dsl" "$BOOT_DIR/acpi_override/dsdt.dsl"
    echo "Created stock BIOS DSDT CPIO archive"
else
    # If no .dat file, just use the .aml
    mkdir -p "$BOOT_DIR/acpi_override"
    cp "$BOOT_DIR/dsdt_acpidump.aml" "$BOOT_DIR/acpi_override/dsdt_acpidump.aml"
    echo "Created stock DSDT CPIO archive (from acpidump)"
fi

# Create CPIO for acpidump version
mkdir -p "$BOOT_DIR/acpi_override_acpidump"
cp "$BOOT_DIR/dsdt_acpidump.aml" "$BOOT_DIR/acpi_override_acpidump/dsdt_acpidump.aml"
cd "$BOOT_DIR/acpi_override_acpidump"
find . | cpio -H newc --create 2>/dev/null > "$BOOT_DIR/acpi_override_acpidump.cpio"
echo "Created acpidump DSDT CPIO: $BOOT_DIR/acpi_override_acpidump.cpio"

# Create CPIO for patched version
mkdir -p "$BOOT_DIR/acpi_override_patched"
cp "$BOOT_DIR/dsdt_patched.aml" "$BOOT_DIR/acpi_override_patched/dsdt_patched.aml"
cd "$BOOT_DIR/acpi_override_patched"
find . | cpio -H newc --create 2>/dev/null > "$BOOT_DIR/acpi_override_patched.cpio"
echo "Created patched DSDT CPIO: $BOOT_DIR/acpi_override_patched.cpio"

# Step 3: Modify /etc/default/grub
echo "Modifying /etc/default/grub..."

GRUB_CONF="/etc/default/grub"
if ! grep -q "GRUB_EARLY_INITRD_LINUX_CUSTOM" "$GRUB_CONF"; then
    echo 'GRUB_EARLY_INITRD_LINUX_CUSTOM="acpi_override_acpidump"' >> "$GRUB_CONF"
    echo "Added GRUB_EARLY_INITRD_LINUX_CUSTOM to $GRUB_CONF"
else
    echo "GRUB_EARLY_INITRD_LINUX_CUSTOM already set"
fi

# Step 4: Create custom GRUB entries
echo "Creating custom GRUB entries..."

CUSTOM_DIR="/etc/grub.d"
CUSTOM_FILE="$CUSTOM_DIR/40_custom"

cat > "$CUSTOM_FILE" << 'EOF'
### BEGIN /etc/grub.d/40_custom ###
# Custom GRUB menu entries for ACPI table injection
# Each entry loads a different DSDT version via CPIO initrd override

# Stock BIOS (no custom DSDT)
menuentry 'Ubuntu (stock BIOS)' --class ubuntu --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-simple-stock' {
    insmod part_gpt
    insmod ext2
    set root='hd0,gpt5'
    search --no-floppy --fs-uuid --set=root de0d4372-382f-4e35-bc99-fd099b027265
    linux	/boot/vmlinuz-6.8.0-136-generic root=UUID=de0d4372-382f-4e35-bc99-fd099b027265 ro acpi_os_name="Windows 2015" acpi_osi="Windows 2015" pcie_aspm=off
    initrd	/boot/initrd.img-6.8.0-136-generic
}

# ACPI DSDT from acpidump
menuentry 'Ubuntu (acpidump DSDT)' --class ubuntu --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-simple-acpidump' {
    insmod part_gpt
    insmod ext2
    set root='hd0,gpt5'
    search --no-floppy --fs-uuid --set=root de0d4372-382f-4e35-bc99-fd099b027265
    linux	/boot/vmlinuz-6.8.0-136-generic root=UUID=de0d4372-382f-4e35-bc99-fd099b027265 ro acpi_os_name="Windows 2015" acpi_osi="Windows 2015" pcie_aspm=off
    initrd	/boot/acpi_override_acpidump.cpio /boot/initrd.img-6.8.0-136-generic
}

# Patched DSDT
menuentry 'Ubuntu (patched DSDT)' --class ubuntu --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-simple-patched' {
    insmod part_gpt
    insmod ext2
    set root='hd0,gpt5'
    search --no-floppy --fs-uuid --set=root de0d4372-382f-4e35-bc99-fd099b027265
    linux	/boot/vmlinuz-6.8.0-136-generic root=UUID=de0d4372-382f-4e35-bc99-fd099b027265 ro acpi_os_name="Windows 2015" acpi_osi="Windows 2015" pcie_aspm=off
    initrd	/boot/acpi_override_patched.cpio /boot/initrd.img-6.8.0-136-generic
}

### END /etc/grub.d/40_custom ###
EOF

echo "Created $CUSTOM_FILE"

# Step 5: Update GRUB
echo "Updating GRUB configuration..."
sudo update-grub

echo ""
echo "=== Setup Complete ==="
echo "GRUB menu entries:"
echo "  - Ubuntu (stock BIOS) - uses stock BIOS DSDT"
echo "  - Ubuntu (acpidump DSDT) - uses acpidump/dsdt.dsl"
echo "  - Ubuntu (patched DSDT) - uses agent_work/dsdt.patched.dsl"
echo ""
echo "To rebuild after changes:"
echo "  sudo /home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers/acpi-inject.sh"
