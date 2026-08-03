#!/bin/bash
# Script to build custom INT3472/TPS68470 module with Chuwi UBook XPro board data
# Requires sudo for copying to kernel build directory and building modules

set -e

# Paths
PROJECT_DIR="/home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers"
AGENT_WORK="${PROJECT_DIR}/agent_work"
KERNEL_BUILD="/lib/modules/6.8.0-136-generic/build"
BUILD_DIR="${KERNEL_BUILD}/drivers/platform/x86/intel/int3472"
OUTPUT_KO="${AGENT_WORK}/intel_skl_int3472_tps68470_custom.ko"

echo "=== Building custom INT3472/TPS68470 module for Chuwi UBook XPro ==="
echo ""

# Step 1: Copy modified source to kernel build directory
echo "[1/3] Copying modified tps68470_board_data.c to kernel build directory..."
sudo cp "${AGENT_WORK}/tps68470_board_data.c" "${BUILD_DIR}/tps68470_board_data.c"
echo "  Copied successfully"

# Step 2: Build the module using kernel build directory
echo ""
echo "[2/3] Building module using kernel build directory..."
sudo make -C "${KERNEL_BUILD}" M="${BUILD_DIR}" intel_skl_int3472_tps68470.ko 2>&1 | tail -20
echo "  Build complete"

# Step 3: Copy the built module
echo ""
echo "[3/3] Copying built module..."
sudo cp "${BUILD_DIR}/intel_skl_int3472_tps68470.ko" "${OUTPUT_KO}"
echo "  Copied to: ${OUTPUT_KO}"

echo ""
echo "=== Module built successfully ==="
echo ""
echo "To load the module:"
echo "  sudo insmod ${AGENT_WORK}/intel_skl_int3472_tps68470_custom.ko"
echo ""
echo "To verify regulators appear:"
echo "  ls /sys/class/regulator/"
echo "  ls /dev/video*"
echo ""
echo "To unload the module:"
echo "  sudo rmmod int3472-tps68470"
