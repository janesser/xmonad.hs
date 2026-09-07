# Task B — `int3472-tps68470` probe failure: root cause

**Date:** Aug 20
**Source:** `20260819_dmesg_ipu3` (custom dump) + boot dmesgs
**Status:** root cause identified (driver/interface mismatch, NOT a BIOS CLDB bug)

## 1. What the dmesg actually shows

- The I2C-based PMIC driver loads as an **out-of-tree module** and then goes **completely silent**:

  ```
  [ 6.333699] intel_skl_int3472_tps68470: loading out-of-tree module taints kernel.
  [ 6.333706] intel_skl_int3472_tps68470: module verification failed: signature and/or required key missing - tainting kernel
  ```

  → no regulator lines, no MFD cells afterwards (matches day8 "loaded but not creating MFD cells / no regulators").

- The sensors **are** enumerated on **I2C2** and are NOT the problem:

  | Sensor | ACPI path | I2C | addr | lanes | DDN |
  |--------|-----------|-----|------|-------|-----|
  | OVTI5648:00 (rear) | \_SB_.PCI0.LNK0 | I2C2 | 0x36 | 2 | GEFF150023R |
  | OVTI2680:00 (front) | \_SB_.PCI0.LNK1 | I2C2 | 0x10 | 1 | GNDF140809R |

- The only live kernel error is a **sensor** deferred probe, *not* the PMIC:

  ```
  [18.577086] i2c i2c-OVTI2680:00: deferred probe pending: ov2680: waiting for fwnode graph endpoint
  ```

## 2. INT3472 is DISCRETE — it has no I2C register space

Custom dump for **both** INT3472:01 and INT3472:02:

- `print_pmic_i2c_dev_name(): (i2c dev not found as expected (DISCRETE))`
- `_CRS` = only `\_SB.PCI0.GPI0` **GPIO** IO resources (3 pins each: 0x78/0x7b/0x7e on DSC0; 0x79/0x7a/0x8f on DSC1), **no I2C resource**.
- CLDB: `version 1, control_logic_type 1` → **"PMIC type is 1: DISCRETE(CRD-D)"** for both.

So `int3472-tps68470` (the I2C-based driver) can't bind: it calls `i2c_get_device()` for the INT3472 I2C address, gets NULL (no I2C resource), and returns early → no regulators.

## 3. Root cause: wrong driver for the PMIC interface

- `int3472-tps68470` is the **I2C-registered** TPS68470 driver. This board's INT3472 is **discrete (CRD-D)** = GPIO + GPIO-regulator, served by **`int3472-discrete` (module alias `intc`)** — which uses CLDB `control_logic_type == 1` and the DiscretePMIC `_DSM` (GUID `6293747A-C5E7-4D63-9778-D8D149229375`).
- The BIOS **already provides `control_logic_type = 1` (DISCRETE)** for both INT3472:01 and :02. The CLDB is already correct — **no CLDB byte needs changing** for this. (The earlier `control_logic_type = 0x02` → `0x01` fix in `DSDT_PATCH_PROPOSAL.md` was based on a wrong assumption about the BIOS; the actual BIOS already emits `0x01`.)

**Fix direction:** bind `intc` / `int3472-discrete` for these devices (GPIO regulators via the discrete `_DSM`), not the I2C `int3472-tps68470`.

## 4. Separate issue (deeper): the ipu3-cio2 CSI host never binds — `_DEP`/graph root cause

`ov2680`/`ov5648` find their I2C device on I2C2 but **defer** — `waiting for fwnode graph endpoint`. Their CSI **graph link to the ipu3-cio2 CSI host** isn't resolved. Both sensors also show:

```
ACPI _DEP: Evaluation failed
```

→ their ACPI `_DEP` (dependency) list fails to evaluate, tied to the CSI graph endpoint not being created. This is an **ipu3-cio2 CSI host / ACPI-graph** problem, independent of the PMIC.

### Why there is no ipu3-cio2 CSI host at all

`ipu3-cio2` **never binds in any captured boot log** (checked `dmesg_with_patched_and_custom_kernel_module_pin_setup2`, its `_efi_cpio` twin, and `20260819_dmesg_ipu3`). The IP3 (the imaging pipeline the CSI host needs) is present at:

```
[  0.663686] pci 0000:00:04.0: [8086:1903] type 00 class 0x118000 conventional PCI endpoint   # multimedia controller = the IP3
[  0.731903] proc_thermal 0000:00:04.0: enabling device (0000 -> 0002)                           # <-- proc_thermal owns it
```

The **only** functional driver for `0000:00:04.0` in the boot log is **`proc_thermal`** (`processor_thermal_device`) — it has **preempted `ipu3-cio2`** on the IP3. `proc_thermal` enables the device for thermal throttling but does **not** create the CIO2 CSI host node nor the fwnode graph endpoints, so:

- no CSI graph endpoint → `ov2680`/`ov5648` defer forever (`waiting for fwnode graph endpoint`);
- no graph endpoint created → their `_DEP` list can't be satisfied (`_DEP: Evaluation failed`).

Note on `20260819_dmesg_ipu3`: the `csi2_data_stream_interface:` lines are the **custom `dump_intel_ipu_data` probe tool** reading the sensor's ACPI `_DSM` CSI data-stream field — **not** a live kernel `ipu3-cio2` probe. There is no `ipu3-cio2`/`connected cameras`/`ip3`/`intc347b` line there either.

`ipu3-cio2.ko` **is installed** on the build host (`/lib/modules/$(uname -r)/kernel/drivers/media/pci/intel/ipu3/ipu3-cio2.ko.zst`) — so the failure is purely that the kernel module never binds because `proc_thermal` grabbed the IP3 first.

**Fix direction for the CSI path:** `proc_thermal` must stop claiming the IP3, and `ipu3-cio2` must bind `0000:00:04.0`. Once ipu3-cio2 binds it creates the CSI graph endpoints and `_DEP` can be satisfied → the sensor deferred probe clears. This is the **harder** half of Task B — it is a driver-binding/ordering conflict, not an AML/CLDB value.

## 5. Bottom line

| Symptom | Root cause | Fix |
|---------|-----------|-----|
| `intel_skl_int3472_tps68470` loads, no regulators | Loaded I2C PMIC driver on a **discrete (GPIO)** PMIC | Load **`int3472-discrete`/`intc`**; CLDB already has `control_logic_type=1` — no CLDB change needed |
| `ov2680: waiting for fwnode graph endpoint` + `_DEP: Evaluation failed` | **`proc_thermal` (processor_thermal) grabbed the IP3 at `0000:00:04.0` (`[8086:1903]`, class `0x118000`), so `ipu3-cio2` never binds → no CSI graph endpoint.** `ipu3-cio2.ko` is installed; this is a driver-binding/ordering conflict, not missing firmware/code. | Make `proc_thermal` release `00:04` so `ipu3-cio2` can bind the IP3 and create the CSI host + fwnode graph endpoints. (Harder half of Task B; separate from the PMIC fix.) |
