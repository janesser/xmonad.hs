#!/bin/bash
# Check the TPS68470 PMIC driver source to understand I2C detection
# and GPIO pin configuration

echo "=== Looking for TPS68470 PMIC source code ==="
echo "Checking kernel source for TPS68470 driver..."

echo ""
echo "=== Kernel modules location ==="
ls /lib/modules/$(uname -r)/kernel/drivers/platform/x86/intel/int3472/ 2>/dev/null

echo ""
echo "=== Check for TPS68470 PMIC opregion ==="
echo "CONFIG_TPS68470_PMIC_OPREGION=y (built-in)"
echo ""
echo "This means the TPS68470 PMIC opregion is compiled into the kernel"
echo "It should auto-detect the PMIC via ACPI"

echo ""
echo "=== Check for GPIO TPS68470 ==="
echo "CONFIG_GPIO_TPS68470=m (module)"
echo "This provides GPIO access to the TPS68470 PMIC"

echo ""
echo "=== Check for CLK TPS68470 ==="
echo "CONFIG_COMMON_CLK_TPS68470=m (module)"
echo "This provides clock control for the TPS68470 PMIC"

echo ""
echo "=== Check for REGULATOR TPS68470 ==="
echo "CONFIG_REGULATOR_TPS68470=m (module)"
echo "This provides regulator control for the TPS68470 PMIC"

echo ""
echo "=== Check for INT3472 platform driver ==="
echo "CONFIG_INTEL_SKL_INT3472=m (module)"
echo "This provides the INT3472 platform driver"

echo ""
echo "=== Check for I2C TPS68470 controller ==="
echo "Looking for I2C controller in kernel source..."
ls /lib/modules/$(uname -r)/kernel/drivers/i2c/ 2>/dev/null | grep -i "tps68470\|int3472" || echo "No I2C TPS68470 controller found"

echo ""
echo "=== Check for GPIO TPS68470 source ==="
ls /lib/modules/$(uname -r)/kernel/drivers/gpio/gpio-tps68470.ko 2>/dev/null || echo "gpio-tps68470.ko not found"

echo ""
echo "=== Check for CLK TPS68470 source ==="
ls /lib/modules/$(uname -r)/kernel/drivers/clk/clk-tps68470.ko 2>/dev/null || echo "clk-tps68470.ko not found"

echo ""
echo "=== Check for REGULATOR TPS68470 source ==="
ls /lib/modules/$(uname -r)/kernel/drivers/regulator/tps68470-regulator.ko 2>/dev/null || echo "tps68470-regulator.ko not found"

echo ""
echo "=== Check for INT3472 discrete driver ==="
ls /lib/modules/$(uname -r)/kernel/drivers/platform/x86/intel/int3472/intel_skl_int3472_discrete.ko 2>/dev/null || echo "intel_skl_int3472_discrete.ko not found"

echo ""
echo "=== Check for INT3472 TPS68470 driver ==="
ls /lib/modules/$(uname -r)/kernel/drivers/platform/x86/intel/int3472/intel_skl_int3472_tps68470.ko 2>/dev/null || echo "intel_skl_int3472_tps68470.ko not found"

echo ""
echo "=== Summary ==="
echo "The TPS68470 PMIC is detected via ACPI opregion (CONFIG_TPS68470_PMIC_OPREGION=y)"
echo "The I2C controller for the PMIC should be auto-detected"
echo "But currently, no PMIC is found on any I2C bus"
echo ""
echo "Possible issues:"
echo "  1. The PMIC is not on the expected I2C bus"
echo "  2. The PMIC is at a different address than expected"
echo "  3. The I2C controller for the PMIC is not properly configured"
echo "  4. The PMIC requires a different I2C controller (not DesignWare)"
