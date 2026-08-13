#!/bin/bash
# Dump ACPI tables and check for INT3472/INT347A/INT347E references

echo "=== DSDT Table Contents (INT347 references) ==="
strings /sys/firmware/acpi/tables/DSDT 2>/dev/null | grep -i "INT347" | head -30

echo ""
echo "=== FACP Table ==="
strings /sys/firmware/acpi/tables/FACP 2>/dev/null | grep -i "gpio" | head -10

echo ""
echo "=== All ACPI Tables ==="
ls /sys/firmware/acpi/tables/

echo ""
echo "=== Check for INT347A and INT347E in all tables ==="
for f in /sys/firmware/acpi/tables/*.dat; do
  echo "--- $f ---"
  strings "$f" 2>/dev/null | grep -i "INT347" | head -5
done

echo ""
echo "=== INT3472 ACPI devices ==="
for dev in /sys/bus/acpi/devices/INT3472:*/; do
  echo "=== $dev ==="
  ls -la /sys/bus/acpi/devices/$dev/
done

echo ""
echo "=== INT347A and INT347E devices ==="
for dev in /sys/bus/acpi/devices/INT347A:*/ /sys/bus/acpi/devices/INT347E:*/; do
  echo "=== $dev ==="
  ls -la /sys/bus/acpi/devices/$dev/ 2>/dev/null
done
