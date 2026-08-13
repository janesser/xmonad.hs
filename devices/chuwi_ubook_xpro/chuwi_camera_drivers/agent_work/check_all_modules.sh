#!/bin/bash
# Check all loaded modules and their dependencies
# Look for any I2C or GPIO related modules

echo "=== All loaded I2C related modules ==="
lsmod 2>/dev/null | grep -i "i2c\|tps68470\|int3472" || echo "None"

echo ""
echo "=== All loaded GPIO related modules ==="
lsmod 2>/dev/null | grep -i "gpio\|tps68470\|int3472" || echo "None"

echo ""
echo "=== Check if any I2C controller is using the int3472-tps68470 driver ==="
ls /sys/bus/i2c/drivers/int3472-tps68470/ 2>/dev/null
echo "Content:"
ls /sys/bus/i2c/drivers/int3472-tps68470/ 2>/dev/null | head -10

echo ""
echo "=== Check if the I2C controller has a firmware_node ==="
for dev in /sys/bus/i2c/drivers/int3472-tps68470/*/; do
  echo "=== $dev ==="
  cat /sys/bus/i2c/drivers/int3472-tps68470/$dev/firmware_node 2>/dev/null
done

echo ""
echo "=== Check for I2C controller parameters ==="
for dev in /sys/bus/i2c/drivers/int3472-tps68470/*/; do
  echo "=== $dev ==="
  ls /sys/bus/i2c/drivers/int3472-tps68470/$dev/ 2>/dev/null
done

echo ""
echo "=== Check for any I2C controller that might be needed ==="
# Look for INT3472 related I2C controllers
for controller in /sys/bus/i2c/drivers/*/; do
  if ls "$controller" 2>/dev/null | grep -q "int3472\|tps68470"; then
    echo "Found: $controller"
  fi
done
