# Research Findings: INT3472/TPS68470 Driver Support for Non-Surface Devices

## 📊 Web Research Summary

### 1. INT3472 Driver Upstream Status
- **Kernel support**: Available in Linux kernels 5.14–7.2+
- **Modules**: `intel_skl_int3472`, `intel_skl_int3472_tps68470`, `intel_skl_int3472_discrete`, `intel_skl_int3472_common`
- **Configuration**: `CONFIG_INTEL_SKL_INT3472=m` (module)
- **Dependencies**: CONFIG_ACPI, CONFIG_COMMON_CLK, CONFIG_I2C, CONFIG_GPIOLIB, CONFIG_LEDS_CLASS, CONFIG_REGULATOR

### 2. Board Data Pattern
The INT3472/TPS68470 driver requires board data for each device with TPS68470 PMIC. Board data includes:
- **Regulator configuration**: Voltages, consumer supplies (DVDD, AVDD, VCM, VIO, VSIO, AUX1, AUX2)
- **GPIO pin mappings**: GPIO chip name ("tps68470-gpio") and pin numbers
- **DMI system info**: Vendor and product name for device identification

**Example (Surface Go):**
```c
static struct gpiod_lookup_table surface_go_int347a_gpios = {
    .dev_id = "i2c-INT347A:00",
    .table = {
        GPIO_LOOKUP("tps68470-gpio", 9, "reset", GPIO_ACTIVE_LOW),
        GPIO_LOOKUP("tps68470-gpio", 7, "powerdown", GPIO_ACTIVE_LOW),
        { }
    }
};
```

### 3. Non-Surface Device Support
- **HP Pro x2 612 G2**: Similar issue (missing board data), still open on linux-surface GitHub
- **Pattern**: Each non-Surface device needs its own board data entry with correct GPIO pins
- **GPIO pins**: Typically 0-6 for TPS68470 PMIC (7 GPIOs total)
- **No Chuwi-specific support**: No upstream support for Chuwi UBook XPro yet

### 4. GPIO Pin Types
The INT3472 discrete driver has GPIO type mappings:
- `INT3472_GPIO_TYPE_RESET` (0x00): "reset" or "enable"
- `INT3472_GPIO_TYPE_POWERDOWN` (0x01): "powerdown"
- `INT3472_GPIO_TYPE_POWER_ENABLE` (0x0b): "power-enable"
- `INT3472_GPIO_TYPE_CLK_ENABLE` (0x0c): "clk-enable"
- `INT3472_GPIO_TYPE_PRIVACY_LED` (0x0d): "privacy-led"
- `INT3472_GPIO_TYPE_HANDSHAKE` (0x12): "handshake" (Meteor Lake+)

### 5. CLDB Buffer Structure
The ACPI CLDB buffer (32 bytes) contains:
- Byte 0: version
- Byte 1: control_logic_type (0x00=ChromeOS, 0x02=Windows/TPS68470)
- Bytes 2-3: control_logic_id, sensor_card_sku
- Bytes 4-13: reserved
- Byte 14: clock_source
- Bytes 15-31: reserved2
- GPIO pins at offsets 0x09-0x0D (C0W1-C0W5 for DSC0)

### 6. Key Upstream Patches
- **[PATCH] platform/x86: int3472: tps68470: add board data for Dell Latitude 5285** - Thierry Chatard
- **[PATCH] platform/x86: int3472: Support multiple gpio lookups in board data** - scpcom
- **[PATCH] platform/x86: int3472: Add board data for Surface Go 3** - googleprodkernel
- **[PATCH] platform: int3472: Add gpio software node** - Antti Laakso

### 7. Community Projects
- **stefanpartheym/archlinux-ipu6-webcam**: Easy installation for patched Intel IPU6 camera drivers
- **linux-surface/linux-surface**: Linux kernel for Surface devices (maintains INT3472 patches)
- **linux-surface/linux-surface/issues/1941**: HP Pro x2 612 G2 board data request (still open)

## 🎯 Key Insights

### 1. GPIO Pin Numbers Vary by Device
- Surface Go uses GPIO 9, 7, 5 for INT347A:00 and INT347E:00
- Non-Surface devices typically use GPIO 0-6
- **Need to determine correct GPIO pins for Chuwi UBook XPro**

### 2. ACPI CLDB Contains GPIO Pin Info
- The CLDB buffer in ACPI tables contains GPIO pin numbers
- Can extract GPIO pin numbers from the ACPI dump
- Our ACPI dump shows CLDB methods but GPIO pin values are in ACPI variables (C0W1-C0W5)

### 3. Board Data Must Match DMI Info
- The DMI system vendor and product name must match exactly
- Chuwi DMI: "CHUWI Innovation And Technology(ShenZhen)co.,Ltd" / "UBook XPro"
- This must be added to the board data table

### 4. Regulator Voltages Are Standard
- CORE (DVDD): 1.2V
- ANA (AVDD): 2.8152V
- VCM: 2.8152V
- VIO: 1.8006V (always_on)
- VSIO: 1.8006V
- AUX1: 2.8152V
- AUX2: 1.8006V

## 📋 Next Steps

### Option A: Extract GPIO Pins from ACPI
1. Use `iasl` to disassemble the ACPI table
2. Find the CLDB method and extract GPIO pin numbers
3. Map ACPI GPIO resources to TPS68470 GPIO pins
4. Add board data with correct GPIO pins

### Option B: Guess GPIO Pins Based on Hardware
1. TPS68470 PMIC has 7 GPIOs (typically 0-6)
2. Common pattern: GPIO 0=reset, GPIO 1=powerdown, GPIO 2=enable
3. Try GPIO 3, 4, 5 for Chuwi (different from Surface Go's 9, 7, 5)
4. Test and iterate

### Option C: Submit Board Data Request to linux-surface
1. Follow the pattern from HP Pro x2 612 G2 issue
2. Provide ACPI dump with CLDB method
3. Request community to add board data

### Option D: Use Windows Driver Information
1. Examine Windows INF files for GPIO pin mappings
2. Windows drivers contain correct GPIO pin numbers
3. Our Windows drivers are in /home/jan/.downloads/UBook XPro/Drivers/ (access issues)

## 🔧 Recommended Approach

**Immediate action**: Try Option B (guess GPIO pins) and test:
- Try GPIO 3, 4, 5 for INT347A:00 (reset, powerdown)
- Try GPIO 6 for INT347E:00 (enable)
- Build custom module and test if regulators appear

**Long-term**: Submit ACPI dump to linux-surface project for proper board data support

## 📁 Key References
- [linux-surface/linux-surface GitHub](https://github.com/linux-surface/linux-surface)
- [HP Pro x2 612 G2 issue #1941](https://github.com/linux-surface/linux-surface/issues/1941)
- [INT3472 driver kernel source](https://github.com/torvalds/linux/tree/master/drivers/platform/x86/intel/int3472)
- [TPS68470 GPIO driver](https://github.com/torvalds/linux/blob/master/drivers/gpio/gpio-tps68470.c)
- [CHUWI Forum: Camera error 10](https://forum.chuwi.com/t/code-error-10-sensor-ov-ubook-xpro/46355)
