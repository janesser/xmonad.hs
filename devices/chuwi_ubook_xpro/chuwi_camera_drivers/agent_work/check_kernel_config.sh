#!/bin/bash
# Check kernel config for I2C and TPS68470 support
# Also check if there's a specific I2C controller needed

echo "=== Kernel Version ==="
uname -r

echo ""
echo "=== Checking kernel config for TPS68470 ==="
grep -i "tps68470\|int3472" /boot/config-$(uname -r) 2>/dev/null

echo ""
echo "=== Checking kernel config for I2C controllers ==="
grep -i "i2c.*controller\|int3472\|tps68470" /boot/config-$(uname -r) 2>/dev/null

echo ""
echo "=== Looking for I2C controller modules ==="
ls /lib/modules/$(uname -r)/modules.builtin 2>/dev/null | grep -i "tps68470\|int3472" || echo "No modules built-in"
ls /lib/modules/$(uname -r)/modules.alias 2>/dev/null | grep -i "tps68470\|int3472" || echo "No module aliases"

echo ""
echo "=== Check for INT3472 I2C controller module ==="
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/int3472-tps68470.ko 2>/dev/null || echo "Module not found"
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/int3472_tps68470.ko 2>/dev/null || echo "Module not found (alt name)"

echo ""
echo "=== Check for GPIO controller modules ==="
ls /lib/modules/$(uname -r)/kernel/drivers/gpio/intel_skl_int3472.ko 2>/dev/null || echo "intel_skl_int3472.ko not found"
ls /lib/modules/$(uname -r)/kernel/drivers/gpio/intel_skl_int3472_gpio.ko 2>/dev/null || echo "intel_skl_int3472_gpio.ko not found"

echo ""
echo "=== Check for any I2C controller that might route to TPS68470 ==="
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/ 2>/dev/null | grep -i "tps68470\|int3472\|tps684" || echo "No TPS68470 I2C controller module"

echo ""
echo "=== Check if I2C controller needs to be manually loaded ==="
lsmod 2>/dev/null | grep -i "tps68470\|int3472"
