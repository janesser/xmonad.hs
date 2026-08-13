#!/bin/bash
# Check I2C buses and try to detect TPS68470 PMIC
echo "=== I2C Bus Scan ==="
for i in 0 1 2 3 4 5 6 7 8; do
  echo "--- i2c-$i ---"
  ls /sys/bus/i2c/devices/i2c-$i/ 2>/dev/null
done

echo ""
echo "=== Scanning for TPS68470 at common addresses ==="
for addr in 0x48 0x49 0x60 0x61; do
  echo "Scanning $addr on all buses..."
  for i in 0 1 2 3 4 5 6 7 8; do
    echo -n "  i2c-$i: "
    cat /sys/bus/i2c/devices/i2c-$i/new_device 2>/dev/null | grep -q "$addr" && echo "FOUND at $addr" || echo "no device"
  done
done

echo ""
echo "=== Loaded I2C Controllers ==="
ls /sys/bus/i2c/drivers/ | grep -i "tps68470\|int3472"

echo ""
echo "=== I2C Controller Bind Status ==="
cat /sys/bus/i2c/drivers/int3472-tps68470/bind 2>/dev/null
