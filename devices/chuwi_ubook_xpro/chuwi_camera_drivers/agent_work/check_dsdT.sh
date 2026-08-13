#!/bin/bash
# Dump and analyze DSDT with iasl to find INT3472 GPIO and I2C routing

echo "=== Dumping DSDT with iasl ==="
iasl -d /sys/firmware/acpi/tables/DSDT 2>&1 > /tmp/dsdT.txt || echo "Failed to dump DSDT"

echo ""
echo "=== Looking for INT3472 in DSDT ==="
grep -n "INT3472" /tmp/dsdT.txt 2>/dev/null

echo ""
echo "=== Looking for INT347A in DSDT ==="
grep -n "INT347A" /tmp/dsdT.txt 2>/dev/null

echo ""
echo "=== Looking for INT347E in DSDT ==="
grep -n "INT347E" /tmp/dsdT.txt 2>/dev/null

echo ""
echo "=== Looking for I2C controllers ==="
grep -n "I2C" /tmp/dsdT.txt 2>/dev/null | head -40

echo ""
echo "=== Looking for GPIO controllers ==="
grep -n "GPIO" /tmp/dsdT.txt 2>/dev/null | head -40

echo ""
echo "=== Looking for Device definitions with INT347 ==="
grep -n "Device.*INT347" /tmp/dsdT.txt 2>/dev/null

echo ""
echo "=== Full DSDT size ==="
wc -l /tmp/dsdT.txt 2>/dev/null
