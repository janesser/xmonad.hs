#!/bin/bash
# Install patched ACPI tables for Chuwi Ubook XPro INT3472 TPS68470
# This patches the CLDB method to set control_logic_type=0x02 instead of 0x00

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DSDT_FILE="${SCRIPT_DIR}/dsdt.patched.dsl"
AML_FILE="${SCRIPT_DIR}/dsdt.patched.aml"
INITRD_DIR="/tmp/patched-acpi"
CPIO_FILE="/tmp/patched_acpi_tables.cpio"

echo "=== Chuwi Ubook XPro ACPI Patch Installer ==="
echo ""

# Step 1: Check prerequisites
echo "[1/5] Checking prerequisites..."
if ! command -v iasl &> /dev/null; then
    echo "ERROR: iasl not found. Install acpica-tools:"
    echo "  sudo apt install acpica-tools"
    exit 1
fi
echo "  iasl: $(iasl --version 2>&1 | head -1)"

# Step 2: Compile the patched ACPI table
echo ""
echo "[2/5] Compiling patched ACPI table..."
if [ ! -f "$AML_FILE" ]; then
    echo "  Compiling ${DSDT_FILE}..."
    iasl -sa "$DSDT_FILE"
    if [ ! -f "$AML_FILE" ]; then
        echo "ERROR: Compilation failed. Check ${DSDT_FILE} for errors."
        exit 1
    fi
    echo "  Created ${AML_FILE} ($(du -h "$AML_FILE" | cut -f1))"
else
    echo "  ${AML_FILE} already exists, skipping compilation"
fi

# Step 3: Create initrd cpio
echo ""
echo "[3/5] Creating initrd cpio..."
mkdir -p "$INITRD_DIR"
cp "$AML_FILE" "${INITRD_DIR}/dsdt.aml"
mkdir -p kernel/firmware/acpi
cp "${INITRD_DIR}/dsdt.aml" kernel/firmware/acpi/dsdt.aml
find kernel -print0 | cpio -H newc --create -0 > "$CPIO_FILE"
echo "  Created ${CPIO_FILE} ($(du -h "$CPIO_FILE" | cut -f1))"

# Step 4: Install for GRUB
echo ""
echo "[4/5] Installing for GRUB..."
if [ -d /boot/efi ]; then
    sudo mkdir -p /boot/efi/EFI/acpi
    sudo cp "$CPIO_FILE" /boot/efi/EFI/acpi/
    echo "  Installed to /boot/efi/EFI/acpi/"
else
    echo "  WARNING: /boot/efi not found. Not installing for GRUB."
    echo "  To install manually:"
    echo "    sudo mkdir -p /boot/efi/EFI/acpi"
    echo "    sudo cp ${CPIO_FILE} /boot/efi/EFI/acpi/"
fi

# Step 5: Verify
echo ""
echo "[5/5] Verifying..."
if grep -q "control_logic_type\|PAR \[One\] = 0x02" "$AML_FILE" 2>/dev/null; then
    echo "  ✓ Patched CLDB methods found in ${AML_FILE}"
else
    echo "  WARNING: Could not verify patch in ${AML_FILE}"
fi

echo ""
echo "=== Installation Complete ==="
echo ""
echo "To apply the patch:"
echo "  sudo reboot"
echo ""
echo "To verify after reboot:"
echo "  dmesg | grep -iE \"TPS68470|regulator|INT3472\""
echo "  ls /sys/class/regulator/ | grep -iE \"dovdd|dvdd|avdd\""
echo ""
echo "To revert (remove initrd and reboot):"
echo "  sudo rm /boot/efi/EFI/acpi/patched_acpi_tables.cpio"
echo "  sudo reboot"
echo ""
