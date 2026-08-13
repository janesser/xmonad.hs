#!/bin/bash
# Check if the int3472-tps68470 I2C controller module needs to be loaded
# and check for any dependencies

echo "=== Current loaded modules ==="
lsmod 2>/dev/null

echo ""
echo "=== Check int3472-tps68470 module ==="
lsmod 2>/dev/null | grep int3472-tps68470 || echo "int3472-tps68470 not loaded"

echo ""
echo "=== Check module dependencies ==="
cat /lib/modules/$(uname -r)/modules.dep 2>/dev/null | grep -i "tps68470\|int3472" | head -20

echo ""
echo "=== Check module alias ==="
grep -i "tps68470\|int3472" /lib/modules/$(uname -r)/modules.alias 2>/dev/null || echo "No alias found"

echo ""
echo "=== Check if int3472-tps68470 is built-in or module ==="
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/int3472-tps68470.ko 2>/dev/null || echo "Not a separate module"
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/int3472_tps68470.ko 2>/dev/null || echo "Not a separate module (alt name)"

echo ""
echo "=== Check for I2C controller modules that might be needed ==="
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/ 2>/dev/null | grep -i "tps68470\|int3472\|tps684" || echo "No TPS68470 I2C controller module"

echo ""
echo "=== Check for GPIO controller modules ==="
ls /lib/modules/$(uname -r)/kernel/drivers/gpio/ 2>/dev/null | grep -i "int3472\|tps68470" || echo "No INT3472 GPIO controller module"

echo ""
echo "=== Try to load int3472-tps68470 if not already loaded ==="
modinfo /lib/modules/$(uname -r)/kernel/drivers/i2c/int3472-tps68470.ko 2>/dev/null || echo "Module not found"
modinfo /lib/modules/$(uname -r)/kernel/drivers/i2c/int3472_tps68470.ko 2>/dev/null || echo "Module not found (alt name)"

echo ""
echo "=== Check if I2C controller needs to be loaded manually ==="
# The int3472-tps68470 I2C controller should be auto-loaded when the PMIC is detected
# But if it's not loaded, it might need to be loaded manually
