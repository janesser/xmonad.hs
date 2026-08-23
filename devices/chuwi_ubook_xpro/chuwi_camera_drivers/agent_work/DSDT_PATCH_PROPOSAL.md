# Better `dsdt.patched.dsl` — INT3472 fix

Artifacts in `agent_work/`:
- `dsdt.patched.dsl`      — prior agent's patch (control_logic_type forced to `0x02`).
- `dsdt.patched.dsl.better` — proposed fix (control_logic_type `0x01`), **compiles clean**
  (`iasl`: 0 Errors; 136 pre-existing Warnings / 468 Remarks unrelated to this change).

## Why the current patch is wrong

The prior patch (`PAR [One] = 0x02`) forces `control_logic_type = 2` (PMIC TPS68470)
on **all 8 INT3472 CLDB methods**. That is wrong on two counts:

1. **The `int3472-discrete` driver REQUIRES `control_logic_type == 1`.**
   `skl_int3472_discrete_probe()` does:
   ```c
   if (cldb.control_logic_type != 1) {
       dev_err(&pdev->dev, "Unsupported control logic type %u\n", ...);
       return -EINVAL;   // <-- 0x02 is rejected
   }
   ```
   So `0x02` makes the discrete driver refuse the device outright.

2. **The BIOS actually emits `1` (DISCRETE), not `2`.** The `20260819_dmesg_ipu3`
   scan (dump_intel_ipu_data) reported `control_logic_type: 1` for the INT3472
   devices, and the camera is found via `ipu3-cio2` — i.e. the discrete/intc path.
   The `int3472-tps68470` (regulator) path also can't work here: DSC0/DSC1 carry
   **GPIO IO resources**, not an I2C resource, so `regmap_init_i2c` fails and the
   board-data lookup (`i2c-INT3472:00`) never matches. That matches the day8 note
   "loaded but not creating MFD cells / no regulators."

## The fix

`PAR [One]` is CLDB byte 1 (`control_logic_type`). Set it to `0x01` (DISCRETE) on
all INT3472 devices. The 8 affected CLDB methods are:

| Device | `_HID` | `_UID` | CLDB uses |
|--------|--------|--------|-----------|
| DSC0 | INT3472 | 0 | C0VE / C0W0..5 |
| DSC1 | INT3472 | 1 | C1VE / C1W0..5 |
| DSC2 | INT3472 | 2 | C2VE / C2W0..5 |
| DSC3 | INT3472 | 3 | C3VE / C3W0..5 |
| CLP0 | INT3472 | 0 | C0VE / C0W0..5 |
| CLP1 | INT3472 | 1 | C1VE / C1W0..5 |
| CLP2 | INT3472 | 2 | C2VE / C2W0..5 |
| CLP3 | INT3472 | 3 | C3VE / C3W0..5 |

(DSC* carry GPIO IO resources; CLP* carry I2C IO resources via `IICB(...)`. Both are
`_HID="INT3472"`. Setting both groups to `0x01` is consistent with the BIOS's own
emission and lets the discrete driver bind them uniformly. CLP* with I2C resources are
handled harmlessly by the discrete driver (no GPIOs mapped, no error); the
`int3472-tps68470` driver is not used.)

The non-INT3472 CLDB methods (e.g. `\_SB.PCI0.I2C2.PMIC`, CLDBs using `L0CV/L1CV…`)
are **not** touched — the global `0x02 -> 0x01` only matched the 8 INT3472 methods.

## What remains firmware/hardware dependent (not patchable in AML)

Even with `0x01`, the discrete driver still needs:
1. **`C0GP > 0`** so `DSC*_._CRS` actually emits the 3 GPIO IO resources
   (built from `C0P0..3` / `C0G0..3` via `PINR`). If the firmware leaves `C0GP=0`
   when the kernel queries `_CRS`, the driver finds no resources ("i2c dev not found").
2. **`_DSM` (GUID `79234640-…`) returning per-pin function data** via the `GPPI(...)`
   calls — this reads the PCH GPIO register space, so it depends on firmware/hardware.
3. **Sensors GPI0/GPI1/GPI2 present & instantiable** (already present, HID INT344B).

The `20260819_dmesg_ipu3` scan shows all three are satisfied in the Windows boot mode
(GPIO resources decoded, 3 pins, sensors enumerated), so `0x01` should be sufficient.

## Optional hardening: deterministic GPIO resources

If you want `_CRS` to emit the GPIO resources regardless of the volatile `C0GP`
field, replace the four `If ((C0GP > N)) { Local1 = PINR(...); ConcatenateResTemplate(...) }`
blocks in each DSC*_._CRS with a fixed GPIO IO resource. This is **not** in
`dsdt.patched.dsl.better` because the exact 18-byte resource layout (resource-source
object handle, pin index, object name) is fragile and the firmware already emits them
correctly in practice. Recommended only if the GPIO path still fails after the `0x01` fix.

## Apply

```bash
iasl dsdt.patched.dsl.better        # -> dsdt.patched.dsl.better.aml
# inject dsdt.patched.dsl.better.aml via the existing initrd flow
```
