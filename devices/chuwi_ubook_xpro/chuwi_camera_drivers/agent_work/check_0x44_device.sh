#!/bin/bash
# Investigate which driver is claiming the device at 0x44 on i2c-3
# and check if it's the TPS68470 PMIC

echo "=== Investigating device at 0x44 on i2c-3 ==="
echo ""

echo "=== i2cdetect scan of i2c-3 ==="
i2cdetect -y 3 2>/dev/null

echo ""
echo "=== Check i2c-3 new_device ==="
cat /sys/bus/i2c/devices/i2c-3/new_device 2>/dev/null

echo ""
echo "=== Check if device is bound to any driver ==="
ls /sys/bus/i2c/drivers/ 2>/dev/null | while read driver; do
  if [ -f "/sys/bus/i2c/drivers/$driver/bind" ]; then
    if grep -q "3" "/sys/bus/i2c/drivers/$driver/bind" 2>/dev/null; then
      echo "Driver $driver has device bound on i2c-3"
      echo "  /sys/bus/i2c/drivers/$driver/bind:"
      cat "/sys/bus/i2c/drivers/$driver/bind" 2>/dev/null
    fi
  fi
done

echo ""
echo "=== Check for TPS68470-specific I2C controller ==="
echo "Looking for I2C controllers that might route to the TPS68470 PMIC..."

echo ""
echo "=== Check for INT3472 I2C controller ==="
ls /sys/bus/i2c/drivers/int3472-tps68470/ 2>/dev/null
echo "Content:"
cat /sys/bus/i2c/drivers/int3472-tps68470/bind 2>/dev/null || echo "Not bound"
echo "Module:"
cat /sys/bus/i2c/drivers/int3472-tps68470/module 2>/dev/null || echo "Not loaded"
echo "Uevent:"
cat /sys/bus/i2c/drivers/int3472-tps68470/uevent 2>/dev/null

echo ""
echo "=== Check for GPIO TPS68470 ==="
ls /sys/class/gpio/gpiochip* 2>/dev/null | grep -i "tps68470" || echo "No TPS68470 GPIO chip"

echo ""
echo "=== Check for CLK TPS68470 ==="
ls /sys/class/clk/clk_tps68470 2>/dev/null || echo "No TPS68470 clock"

echo ""
echo "=== Check for REGULATOR TPS68470 ==="
ls /sys/class/regulators/regulator_tps68470* 2>/dev/null || echo "No TPS68470 regulator"

echo ""
echo "=== Check for INT3472 platform device ==="
ls /sys/bus/platform/devices/ 2>/dev/null | grep -i "INT3472\|tps68470" || echo "No INT3472 platform device"

echo ""
echo "=== Check for I2C devices on i2c-3 ==="
ls /sys/bus/i2c/devices/i2c-3/ 2>/dev/null

echo ""
echo "=== Try to read the device name ==="
# The device should have a name file
cat /sys/bus/i2c/devices/i2c-3/new_device 2>/dev/null
echo ""
echo "Device name: $(cat /sys/bus/i2c/devices/i2c-3/new_device 2>/dev/null)"

echo ""
echo "=== Check for I2C controller that routes to i2c-3 ==="
echo "i2c-3 is SMBus I801 adapter at f040"
echo "This is the ICH SMBus controller"
echo ""
echo "Possible I2C controllers:"
echo "  1. ICH SMBus (i2c-3) - This is where the device is"
echo "  2. DesignWare I2C controllers (i2c-0, i2c-1, i2c-2)"
echo "  3. GMBUS controllers (i2c-4 to i2c-8)"
echo ""
echo "The TPS68470 PMIC should be on the ICH SMBus (i2c-3)"

echo ""
echo "=== Check if the PMIC is at a different address ==="
echo "Scanning all I2C buses for common PMIC addresses..."
for addr in 0x44 0x48 0x49 0x60 0x61 0x36 0x37 0x38 0x39; do
  echo -n "Address $addr: "
  for i in 0 1 2 3 4 5 6 7 8; do
    if cat /sys/bus/i2c/devices/i2c-$i/new_device 2>/dev/null | grep -q "$addr"; then
      echo "FOUND on i2c-$i"
      break
    fi
  done
done

echo ""
echo "=== Summary ==="
echo "Device at 0x44 on i2c-3 is likely the TPS68470 PMIC"
echo "It's bound to multiple drivers: as3711, da903x, da9063, int3472-tps68470, max310x, max77693, rc5t583"
echo "This suggests the PMIC is being claimed by multiple drivers"
echo ""
echo "Possible issues:"
echo "  1. The PMIC is not responding correctly"
echo "  2. The I2C controller (SMBus I801) is not properly configured"
echo "  3. The INT3472 ACPI driver doesn't have the correct GPIO/I2C routing"
echo "  4. The PMIC requires a different I2C controller"
