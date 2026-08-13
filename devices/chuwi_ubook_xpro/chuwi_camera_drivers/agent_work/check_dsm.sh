#!/bin/bash
# Check DSM (Device-Specific Methods) for INT3472 devices
# This can reveal I2C routing and GPIO configuration

echo "=== Checking for DSM methods on INT3472 devices ==="
for dev in /sys/bus/acpi/devices/INT3472:*/; do
  echo "=== $dev ==="
  ls -la /sys/bus/acpi/devices/$dev/
  echo "dsm:"
  cat /sys/bus/acpi/devices/$dev/dsm 2>/dev/null
  echo "dst:"
  cat /sys/bus/acpi/devices/$dev/dst 2>/dev/null
done

echo ""
echo "=== Checking for DSM methods on INT347A ==="
ls /sys/bus/acpi/devices/INT347A:*/ 2>/dev/null
cat /sys/bus/acpi/devices/INT347A:*/dsm 2>/dev/null

echo ""
echo "=== Checking for DSM methods on INT347E ==="
ls /sys/bus/acpi/devices/INT347E:*/ 2>/dev/null
cat /sys/bus/acpi/devices/INT347E:*/dsm 2>/dev/null
