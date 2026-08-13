#!/bin/bash
# Try to manually scan I2C buses for TPS68470 PMIC
# This bypasses the kernel driver and directly probes for the PMIC

echo "=== Manual I2C bus scan for TPS68470 ==="
echo "Scanning all I2C buses for devices at common PMIC addresses..."
echo ""

for i in 0 1 2 3 4 5 6 7 8; do
  echo "=== i2c-$i ==="
  devices=$(ls /sys/bus/i2c/devices/i2c-$i/ 2>/dev/null | grep -v "i2c-dev")
  if [ -z "$devices" ]; then
    echo "  (empty)"
  else
    for dev in $devices; do
      echo "  Found: $dev"
      cat /sys/bus/i2c/devices/$dev/name 2>/dev/null
    done
  fi
done

echo ""
echo "=== Try to detect TPS68470 using i2cdetect ==="
which i2cdetect 2>/dev/null || echo "i2cdetect not found"
if command -v i2cdetect &>/dev/null; then
  i2cdetect -y 3 2>/dev/null || echo "i2cdetect failed (no i2c-tools or no permissions)"
fi

echo ""
echo "=== Check for i2c-tools ==="
ls /usr/bin/i2c* 2>/dev/null || echo "No i2c-tools installed"

echo ""
echo "=== Try to read I2C bus info ==="
for i in 0 1 2 3 4 5 6 7 8; do
  echo "=== i2c-$i info ==="
  cat /sys/bus/i2c/devices/i2c-$i/name 2>/dev/null
  cat /sys/bus/i2c/devices/i2c-$i/new_device 2>/dev/null
  cat /sys/bus/i2c/devices/i2c-$i/uevent 2>/dev/null | head -5
done
