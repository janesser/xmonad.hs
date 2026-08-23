# INT3472 on Linux — DSDT analysis (what needs to change)

Ground truth sources checked:
- `acpidump/dsdt.dsl` (full 43k-line table, lines ~37384–37960 = DSC0/DSC1, ~15797 = GPI0)
- `acpidump/dsdt.cam0_cam1.dsl` (curated CAM0/CAM1/PMIC excerpt)
- `20260819_dmesg_ipu3` (runtime evidence; booted with acpi_os_name=Windows 2015)
- `linux-source-6.8.0/drivers/platform/x86/intel/int3472/{discrete.c,tps68470.c,common.c,tps68470_board_data.c}`
- `linux-source-6.8.0/drivers/media/pci/intel/ipu3/ipu3-cio2.c`

## TL;DR
The DSDT **already contains the INT3472 devices in the layout the Linux kernel expects** (two `\_SB.PCI0.DSC0`/`DSC1` devices, HID `INT3472`, with GPIO IO resources in `_CRS`, a per-pin `_DSM`, and CLDB). The blocker is **not the AML syntax** — it's firmware/boot mode and a few field values. Two separate "layouts" exist in the same tree and they point at two different driver paths; the BIOS actually emits the one that is Linux-native, but the curated excerpt is the Windows/I2C one.

## A. What 20260819_dmesg_ipu3 actually shows
Captured with `acpi_os_name=Windows 2015 acpi_osi=Windows 2015`. A Windows-side scanner (`dump_intel_ipu_data`) read the ACPI and reported (timestamps ~4580):

| Field | Value |
|---|---|
| Device names | `INT3472:01` = `\_SB_.PCI0.DSC0`, `INT3472:02` = `\_SB_.PCI0.DSC1` |
| `_HID` / `_CID` | `INT3472` |
| `_DDN` | `PMIC-CRDG` |
| **CLDB `control_logic_type`** | **1 = DISCRETE(CRD-D)** (NOT 2 = TPS68470) |
| GPIO pins | 3 per device; per-pin function data in `_DSM` (GUID `79234640-9e10-4fea-a5c1-b5aa8b19756f`) |
| Sensors seen | `i2c-OVTI5648:00`, `i2c-OVTI2680:00` (via the IPUCIU/cio2 path) |

Kernel side in the same boot: `ipu3-cio2` found OV2680 and "Connected 1 cameras"; **no INT3472 regulator activity**. So:
- The BIOS emits INT3472 as **DISCRETE**, so the relevant driver is **`int3472-discrete`**, not `int3472-tps68470`.
- The `int3472-tps68470` driver never logs → it matched nothing usable (its I2C probe can't work on a GPIO-only device, and its board_data lookup by name fails).

## B. The two layouts in the DSDT (both present)
1. **DSC0/DSC1 — discrete (the BIOS's real output).** `_HID=INT3472`, `_CRS` returns GPIO IO resources built from `C0GP`/`C0P0..3`/`C0G0..3` via `PINR`, `_DSM` with GUID `79234640…` returns per-pin data via `GPPI(...)`, `CLDB` byte 1 = `C0TP`. Sensors `GPI0/GPI1/GPI2` are **interrupt-driven MMIO** devices (HID `INT344B`/`INT3451`/`INT345D`). → handled by `int3472-discrete` + `ipu3-cio2`. This is the Linux-native path.
2. **CAM0/CAM1/PMIC — I2C (hand-curated `dsdt.cam0_cam1.dsl`).** `Device(PMIC)` named "PMIC" on `\_SB.PCI0.I2C2` @ 0x4C, cameras as separate I2C devices (IMX135/OV2740). → `int3472-tps68470` (regulator/MFD) + ov2680/imx135/ov2740. This is a Windows-style abstraction and does not match what the BIOS actually emits.

## C. What must change for INT3472 to "work properly" on Linux

### Discrete path (what the BIOS emits) — mostly fine, fix these:
1. **`C0TP` (CLDB byte 1) must be 1 (DISCRETE), not 0.** It's an 8-bit MMIO field in `DSC0.CLDB`. If the firmware leaves it 0, the `int3472-tps68470` regulator driver mis-routes to ChromeOS (no regulators). Set `C0TP=0x01` in firmware, or ensure the discrete path is what runs.
2. **`C0GP` (number of GPIOs) must be > 0.** `DSC0._CRS` only emits GPIO IO resources when `C0GP > 0`. If 0, the discrete driver finds no resources → "i2c dev not found as expected (DISCRETE)". Confirm the PCH registers expose 3 GPIOs.
3. **`_DSM` (GUID `79234640-…`) must return real per-pin function data.** Present via `GPPI(...)`; verify it returns non-zero type/pin/active-value for the Chuwi (currently the scan couldn't read the i2c-device-count GUID).
4. **Parent bus `\_SB.PCI0` must be instantiated on Linux** so `DSC0`/`DSC1` become Linux devices. If the PCH/PCI bus isn't enumerated in the (patched) ACPI, the INT3472 devices never appear at all.

### I2C / TPS68470 path (only if you deliberately want that driver) — needs real changes:
1. **Rename the device `PMIC` → `INT3472`** so the i2c-acpi client is `i2c-INT3472:00` (board_data looks up exactly that; "PMIC" yields `i2c-PMIC:00`).
2. **Change I2C address `0x4C` → `0x00`** so the client name matches the Chuwi board_data entry `dev_name="i2c-INT3472:00"`. With 0x4C the name is `i2c-INT3472:4C` → `int3472_tps68470_get_board_data()` returns NULL → "No board-data found for this model".
3. **CLDB `control_logic_type` = 0x02** (TPS68470/Windows path).
4. Chuwi board_data entry already exists in `tps68470_board_data.c` (`chuwi_tps68470_board_data`), so no new code is needed once the name/address line up.

### The deeper conceptual point
The cameras on this board are **interrupt-driven MMIO sensors (GPI0/GPI1/GPI2)**, not I2C sensors. The CAM0/CAM1 IMX135/OV2740 "I2C devices" in `dsdt.cam0_cam1.dsl` are a **Windows-only abstraction** with no real Linux-enumerated I2C sensor behind them. So `PLAN.md`'s `imx135_init()`/`ov2740_init()` over I2C targets the wrong sensor model — the working path is the discrete/intc + `ipu3-cio2` enumeration the dmesg already shows.

## D. Minimal DSDT patch (discrete path)
- Ensure `C0TP` field = 1 and `C0GP` = 3 in the INT3472 MMIO region.
- Keep `_HID="INT3472"`, the GPIO IO `_CRS` (from PINR), the `79234640…` `_DSM` with `GPPI(...)`, and `CLDB`.
- Make sure `\_SB.PCI0` (PCH) is present in the ACPI the kernel parses.
- Compile with `iasl -sa dsdt.dsl` and inject the `.aml` via initrd (the existing `install-acpi-patch.sh` / `ACPI-PATCH-README.md` flow).
