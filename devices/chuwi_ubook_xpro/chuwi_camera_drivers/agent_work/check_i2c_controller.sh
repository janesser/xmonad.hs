#!/bin/bash
# Check the int3472-tps68470 I2C controller configuration
# and try to manually scan I2C buses

echo "=== int3472-tps68470 controller info ==="
cat /sys/bus/i2c/drivers/int3472-tps68470/module 2>/dev/null
cat /sys/bus/i2c/drivers/int3472-tps68470/bind 2>/dev/null

echo ""
echo "=== Try to read I2C controller params ==="
# Check if there are any I2C controller devices
ls /sys/bus/i2c/devices/ | grep -v "i2c-dev" | while read dev; do
  echo "--- $dev ---"
  cat /sys/bus/i2c/devices/$dev/name 2>/dev/null
  cat /sys/bus/i2c/devices/$dev/firmware_node 2>/dev/null
  cat /sys/bus/i2c/devices/$dev/software_node 2>/dev/null
done

echo ""
echo "=== Check for I2C controllers in device tree ==="
for f in /sys/firmware/acpi/tables/*.dat; do
  strings "$f" 2>/dev/null | grep -i "I2C\|I2C\s*\(" | head -20
done

echo ""
echo "=== Check kernel config for I2C controllers ==="
grep -i "INT3472\|int3472\|tps68470" /boot/config-$(uname -r) 2>/dev/null | head -20
