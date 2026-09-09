# Day 15 — Windows ACPI dump cracked: this is an Intel **CRD** PMIC, and the kernel's `int3472-discrete` driver does not speak that convention

**Date:** 2026-09-08 (camera case, Chuwi UBook XPro)
**Artifacts:** `acpidump.win/` (real firmware DSDT + 13 SSDTs, dumped from the dual-boot Windows boot), kernel source `linux-source-6.8.0/drivers/platform/x86/intel/int3472/`, `reveng_skc_gpio_pins.md`.
**Supersedes:** day13 DMA notes, day14 "C0* pin reconstruction" hypothesis. **Both are now known to be based on the wrong DSDT.**

---

## TL;DR

The missing power-config SSDT day-14 was hunting for **does not exist as a separate table**.
Everything is inside the DSDT — and the DSDT is an **Intel CRD (Common Reference Design)
TPS68470** design. The Linux kernel driver that binds (`int3472-discrete`, alias `intc`)
**cannot** create the camera rail or MCLK from a CRD DSDT, because:

1. The CRD PMIC `_CRS` exposes **only an I2C bus — no GPIO lines**. The driver only makes
   rails/clocks out of GPIO resources in `_CRS` (or the `_DSM` GUID `79234640`), which are absent.
2. The CRD `_DSM` GUIDs in the firmware (`49752526-…`, `8fce2a82-…`, `4cbb43c4-…`) are **not** the
   GUIDs the driver looks for (`79234640-…` for GPIO type, `82c0d13a-…` for the DSM clock).
3. `SCSS` (the `_STA` gate that makes the PMIC visible) is an **External object set by the
   SCC/EC** — 1 in Windows, 0 on Linux → PMIC hidden (`_STA=0`) → driver can't even probe it.

So the failure is a **driver ↔ firmware-convention mismatch + an EC power-enable signal with no
Linux equivalent**, not a missing SSDT and not a DMA problem.

---

## 1. What the Windows dump actually is

`acpidump.win/acpidump.txt` is a raw hex blob of 27 ACPI tables (1 DSDT, 13 SSDT, FADT, APIC,
DMAR, …). I split it into per-table `.dat`, then hand-disassembled the camera devices by decoding
the AML byte streams directly (the DSDT is ~183 KB of CRD data; upstream `iasl` 20230628 trips on
one malformed `Package` at `~0x270C` in the pre-cameras preamble, but the camera region parses fine).

### Camera topology (decoded straight from the firmware DSDT)

| Device | `_HID` | `_DDN` (firmware, *old names*) | I2C bus | addr | `_DEP` |
|--------|--------|-------------------------------|---------|------|--------|
| CAM0   | INT3471 | IMX135-CRDG2                  | `\_SB.PCI0.I2C2` | **0x10** | {PMIC} |
| CAM1   | INT3474 | OV2740-CRDG2                  | `\_SB.PCI0.I2C4` | **0x36** | {PMIC} |
| PMIC   | INT3472 | PMIC-CRDG2                    | `\_SB.PCI0.I2C2` | **0x4c** | — |

- **CAM0 `_CRS`** (`0x29aed`) = `I2cSerialBusV2` at addrs **0x10, 0x0e, 0x50, 0x51, 0x52** on
  `\_SB.PCI0.I2C2` (main sensor = 0x10). Only I2C — **no GPIO**.
- **CAM1 `_CRS`** (`0x29d7d`) = `I2cSerialBusV2` at **0x36** on `\_SB.PCI0.I2C4`. Only I2C.
- **PMIC `_CRS`** (`0x29f2b`) = `I2cSerialBusV2` at **0x4c** on `\_SB.PCI0.I2C2`. Only I2C.
- All three `_STA` methods return `0x0f` when **`Store(SCSS, 1)`** holds. `SCSS` is defined
  **nowhere** in the DSDT (3 hits, all inside `_STA`) — it is a truly External object.

### CRD signatures in the DSDT

- `_CLDB` present on PMIC → control-logic data (Windows `dump_intel_ipu_data` read
  `control_logic_type = 1` / "DISCRETE/CRD-D").
- `_DSM` methods carry GUIDs **`49752526-7192-a44c-bb43-c4899d5a4881`** (×10, the CRD SSDTRM),
  **`8fce2a82-1428-7441-a56b-5f029fe079ee`** (×6), **`4cbb43c4-899d-5a48-81a0-0b936a00a411`**.
- `SCSS` is asserted by the **SCC/EC**, not by the DSDT. Confirmed by
  `reveng_skc_gpio_pins.md`: the Windows `SkcController.sys` drives camera power/clock through an
  EC GPIO register bank — `Power0`(0x14001b210), `Power1`(0x14001b220), and `PowerEn`/`Mclk`
  (0x14001b240) — the Linux side has no equivalent driver, so `SCSS` stays 0.

---

## 2. Root cause, pinned against the kernel driver

Kernel driver `intel_skl_int3472_discrete` (`discrete.c`, `clk_and_regulator.c`) probe path:

1. `skl_int3472_fill_cldb()` → needs `control_logic_type == 1`. **CRD passes** (Windows = 1).
2. `skl_int3472_parse_crs()` → `acpi_dev_get_resources()` +
   `skl_int3472_handle_gpio_resources()`. For **each GPIO** in the PMIC `_CRS` it:
   - reads the GPIO type from `_DSM` **GUID `79234640-9e10-4fea-a5c1-b5aa8b19756f`**,
     function `ngpios+2`; byte0 = type (`0x0b`=POWER, `0x0c`=CLK).
   - `0x0b` → registers a GPIO regulator (`skl_int3472_register_regulator`),
     `0x0c` → registers a GPIO clock.
   - **CRD has no GPIO in `_CRS`** → the loop never runs → **no regulator, no GPIO clock.**
3. `skl_int3472_register_dsm_clock()` → only registers a DSM clock if
   `acpi_check_dsm(adev, &img_clk_guid=82c0d13a-…, 0, BIT(1))`. **CRD uses `49752526`, not
   `82c0d13a`** → returns false → **no DSM clock either.**

Net: `intc` binds (matches `INT3472`, CLDB type 1) but creates **zero** rails and **zero**
clocks. Combined with `SCSS == 0` on Linux (PMIC hidden, `_STA=0`, so the driver can't even probe
the camera PMIC), this is exactly the day-14 symptom: OV2680 on `reg-dummy`, no MCLK, media link
disabled, `STREAMON → "Link severed"`.

**This is a firmware-convention gap in the kernel, not a board defect.** The mainline
`int3472-discrete` driver implements the *classic* INT3472 GPIO-in-`_CRS` convention
(GUID `79234640` / `82c0d13a`), not the *CRD/SSDTRM* convention this Chuwi firmware uses
(GUID `49752526` / `8fce2a82` / `4cbb43c4`).

---

## 3. What the wrong-DSDT history explains

The repo's `dsdt.cam0_cam1.dsl` (used by day-7/14 analysis) used the **classic** GUID
`26257549-9271-4ca4-bb43-c4899d5a4881` with `PINR(C0P#, C0G#)` and `External (C0GP/SCSS)`.
That file is a **hand reconstruction, not the real firmware table**. The real firmware is CRD.
So:
- The "C0\* pin VALUES are External, defined in a missing power SSDT" question is **moot** —
  CRD has no `C0*` globals; pins live in the CRD `_DSM`/SSDTRM instead.
- `fix/proc_thermal_release_ip3.sh` was already retired (day-14 §3); keep it retired.

---

## 4. Fix options (Architect/Dev decision)

Ranked by blast radius on a chezmoi-managed daily driver:

- **A. Make `SCSS` true on Linux so the PMIC is visible.** Even if we never get full CRD rail
  mapping, a *visible* PMIC + a userspace `gpio-regulator` (device-tree/gpio-regulator binding,
  works from userspace, no kernel rebuild) that maps AVDD/DOVDD/DVDD + an `fwnode` clock for MCLK
  could stream. This is the **most reversible / least invasive**: a small board-specific
  `gpio-regulator` + clk node, no DSDT rebuild, no kernel module patch.
  - Blocker (RESOLVED — see §6): the SCC is a **firmware/ACPI-run controller** (register blocks
    `CDW1pCTRL`/`CDW3pCTRL`, sub-tables `SKCT`/`SKC3`/`MEM3`…) that owns the camera-power MMIO
    `Power0`(0x14001b210)/`Power1`(0x14001b220)/`PowerEn`+`Mclk`(0x14001b240). It is **not a PCI
    device** on Linux and the boot EC is **port-mapped** with **no debugfs memory shadow**, so
    there is **no currently-accessible Linux interface** to assert `OSCC`/`SCS_` or write the
    SCC power register. → Option A's only missing piece is exactly the piece with no userspace
    equivalent. See the verdict in §6.
- **B. Add CRD SSDTRM support to `int3472-discrete`.** Teach the driver the `49752526`/`82c0d13a`
  GUIDs and read rail/clock pins from the SSDTRM `_DSM`. Correct long-term, but requires a kernel
  patch + rebuild + signed-module/secure-boot handling on a production box — **highest blast
  radius.**
- **C. DSDT SSDT patch** to rewrite the CRD PMIC so its `_CRS` carries real GPIO lines and the
  driver's `79234640` path works. Fragile across BIOS/kernel updates; avoid on a chezmoi box.

**Recommended for a proof-of-streaming ASAP:** the honest path is **B** (add CRD SSDTRM support
to `int3472-discrete` + drive the SCC power-enable) as the durable fix, with **A** as an interim
userspace shim *only if* a safe SCC power-enable surface exists. Given §6, **A has no safe
usersurface**, so start from **B**.

---

## 6. EC / SCC power investigation (2026-09-09)

Goal: can we assert `OSCC`/`SCS_` (the PMIC visibility gate) from Linux **without a kernel
module** — the missing piece for Option A?

**Findings:**

- **The gate is `CL00 && (C0TP == One)` — NOT `OSCC`.** Disassembled the real firmware DSDT
  (see below). INT3472 PMIC `_STA` (device `DSC0`):
  ```aml
  Method (_STA) {
      If (CL00) { If ((C0TP == One)) { Return (0x0F) } }
      Return (Zero)
  }
  ```
  `CL00`/`C0TP` are **CRD SSDTRM control-logic globals** — `External` in the DSDT top (lines
  1041/1050) but **assigned nowhere** in the DSDT or any of the 13 extracted SSDTs. Set only by
  the CRD control-logic handshake Windows runs. On Linux they stay `0` → `_STA=0` → PMIC hidden.
- **`OSCC` is set by `_SB.PCI0._OSC`** (PCI Host Bridge Device OSC, GUID
  `33db4d5b-1ff7-401c-9657-7441c03dd766`), doing `OSCC = CTRL = CDW3` from the OS capability
  handshake. `OSCC` is consumed only for **PCI host-bridge power** (NHPG/NPME in the `RWAK`
  wake method) — NOT for the camera PMIC. So "OSCC gates the PMIC" was a misread.
- `strings` also shows the SCC as real AML objects: `SKCT`, `SKC3`, `SAC3`, `MEM3`, `AMC3`,
  `EFC3`, `VRC3`, `WFC3`, `G1C3`… register blocks `CDW1pCTRL` / `CDW3pCTRL`, and a **`Power
  Sharing Manager`** device at `\_SB.PCI0` `0x2700` (`_STR` = "Power Sharing Manager").
  `CDW1pCTRL`/`CDW3pCTRL` are the SCC register block `SkcController.sys` programs
  (`Power0`=0x210, `Power1`=0x220, `PowerEn`+`Mclk`=0x240).
- **Readable-DSDT method:** iasl 20230628 desyncs on 5 corrupt opcodes; NoOp-patching
  (`0xFF`) at `0x271C,0x271D,0xA5CE,0xF94A,0x15516` yields a full disassembly
  (`DSDT_patch.dsl`, 1.4 MB, 43k lines) in `acpidump.win/win_tables/`.
- **SCC is NOT a PCI device on Linux.** Every device under `/sys/bus/pci/devices` is Intel PCH
  (`00:14.3`=ipu3-cio2, `00:15.0-2`=I2C, `00:14.2`/`00:16.0`=MEI). No `v*d` for the SCC → no
  sysfs/MMIO region to poke from userspace.
- **Boot EC is port-mapped** (`0x66`/`0x62`, GPE `0x50`). `ec_sys` loads (rc 0) but creates **no
  `/dev/ec_sys`** (needs a memory region; this EC has none).
- **debugfs `ec/ec0` exposes only `gpe`(=0x50), `io`, `use_global_lock`** — **no** `os`/`bios`/
  `read_only`/`write_only` shadow files. The `io` file is just an I/O-region snapshot. So EC
  register bytes (including the SCC camera-power words) are **not readable** via debugfs.

**Verdict:** Option A's missing piece is actually **two** deep firmware-handshake gates, neither of
which is userspace-settable:
1. **CRD control-logic adoption** (`CL00 && C0TP==1`) — set only by a Windows-only CRD SSDT/OSCC
  handshake. Not in any Linux-visible table → can't be triggered from userspace.
2. **SCC camera-power enable** (`Power0`/`Power1`/`PowerEn` MMIO) — the SCC is a firmware
  controller with no PCI device, and the port EC gives no register access (no memory shadow).
Reaching either would require reimplementing the CRD/SSDTRM adoption + SCC protocol (from
`SkcController.sys`) or raw `iopl` on `0x62`/`0x63` (dangerous on a daily driver). **Both are out
of bounds for the stated constraints.**

**Therefore the durable fix is B:** teach `int3472-discrete` the CRD `49752526`/`8fce2a82`
system-PMIC `_DSM`/SSDTRM convention (read rails + MCLK pin from SSDTRM) **and** drive the SCC
power-enable (`Power0`/`Power1`/`PowerEn`). This is a kernel change → rebuild + signed-module /
secure-boot handling on the production box (the highest-blast-radius option, but the only correct
one). A and C remain viable only if a safe SCC power-enable surface is later exposed.

> Open question for the Architect (Winston): is the SCC reachable via the **Intel MEI**
> (`00:14.2`/`00:16.0`) — i.e. is there an ME-side interface we could use instead of raw EC
> register poking? That would decide whether B needs an ME driver, an EC driver, or just the
> int3472 CRD patch.

### 6b. CRD adoption is the true blocker for Option B (2026-09-09)

Disassembling the readable DSDT (`DSDT_patch.dsl`) pinned the exact contract and revealed a
**harder blocker than raw SCC access**:

- **The INT3472 PMIC `_STA` gates on `CL00 && (C0TP == One)`.** `CL00`, `C0TP`, `C0GP`, `C0CV`
  and `C0W0-5` are CRD SSDTRM globals declared `External` at the DSDT top (lines 1041-1066) but
  **assigned nowhere** in the DSDT or any of the 13 extracted SSDTs. They are written solely by
  the Windows `SkcController.sys` → SCC. On Linux they stay `0` → `_STA = 0` → the PMIC is **not
  enumerated at all** (`INT3472:00/status = 0`). The kernel therefore never reaches `probe`, so
  the whole `int3472-discrete` init (CLDB check, GPIO parse, regulator/clk creation) is skipped.
- **The CRD PMIC `_DSM` reuses the classic `79234640` GUID** but with a *different* return
  format the current driver can't consume:
  - fn 0 → `Buffer {0x3F}` (classic expects nothing here),
  - fn 1 → `C0GP` (GPIO count),
  - fn 2… → **`GPPI(...)` GpiConsumer packages**, one per pin (classic returns a 32-bit pin-type
    int per `79234640` fn `ngpios+2`).
  The driver only calls `79234640` from inside the `_CRS`-resource loop, and the CRD `_CRS`
  embeds pins as `PINR(C0P#, C0G#)` gated on the (zero) `C0GP` — so the loop body never runs.
- **No `49752526` (SSDTRM) GUID exists in the firmware we have.** The "CRD SSDTRM"
  adoption handshake is a Windows-only code path; there is no Linux-visible table or OSC that
  triggers `CL00`/`C0TP`.

**Implication:** Option B is *not* "add an SSDTRM GUID to the driver." It is a kernel change that
must (1) make the PMIC **visible** by triggering `CL00`/`C0TP`, and (2) parse the CRD `_DSM`/`_CRS`
GPIO convention. Step (1) is the blocker: it requires driving the SCC — no PCI device, port EC
with no memory shadow — i.e. reimplementing `SkcController.sys` in-kernel. Without visibility,
the whole `probe` path never starts. See §6 verdict + Winston's MEI question.

**Refined Option C (worth the Architect's look):** an ACPI **SSDT patch/injection** that (a) forces
the camera PMIC `_STA` to `0x0F` and (b) exposes its pins as classic `GpiConsumer` +
`79234640` so the *existing* `int3472-discrete` works unchanged. This sidesteps both SCC access
*and* the CRD-`_DSM` parser, but still needs root ACPI-override on the daily driver.

---

## 5. LIVE empirical verification (2026-09-09)

Boot `peacewagon`, kernel 6.8.12, `ov2680` bound. sudo NOPASSWD rule still covers only the
10 camera tools (i2cdetect/i2cset/i2cget/i2ctransfer/media-ctl/v4l2-ctl/modprobe/rmmod/acpidump/
iasl) — NOT lsmod/dmesg/cat/journalctl. Read kernel log via `journalctl -k` (jan ∈ `adm`).

Results, all reproducible now:

| Check | Result |
|-------|--------|
| `ov2680` probe log | `supply DOVDD/DVDD/AVDD not found, using dummy regulator`; `sensor_revision id = 0x2680` |
| `ls /sys/class/clk` | **absent** — no MCLK provider is registered |
| `regulator.0` | `reg-dummy` only — no real PMIC rail |
| `/sys/bus/acpi/devices/INT3472:00/status` | **0 (hidden)** — camera PMIC hidden by `SCSS==0` |
| `/sys/bus/acpi/devices/INT3472:01,02/status` | **15 (enabled)** (GPIO-expander instances) |
| `/sys/bus/i2c/devices/i2c-OVTI2680:00/supplier` | `regulator:regulator.0` (dummy) |
| I2C2 `i2cdetect -y 2` | blank scan, BUT `i2cget -y 2 0x10` → *Device or resource busy* (driver owns bus) |
| media0 graph | `ov2680 2-0010:0 -> ipu3-csi2 1:0` link **present but DISABLED** (`[]`) |
| `v4l2-ctl -d /dev/video1 --stream-mmap` | **`VIDIOC_STREAMON returned -1 (Link has been severed)`**, 0 bytes captured |

**This closes the loop on day-14.** The chain is: `SCSS==0` (EC-set, no Linux equivalent) →
`INT3472:00` hidden → `int3472-discrete` never binds the camera PMIC → no rail + no MCLK clock
provider → ov2680 on `reg-dummy`, media link disabled → STREAMON "Link severed".

Note on the `i2cdetect` blank scan: a plain SMBus scan sees nothing because the sensor is a live
client the driver already has open (`EBUSY`), yet it read its chip id at probe — so I2C2 is
*powered and functional*; the gap is power rails + clock, not the I2C bus.

## 6. Next experiments (root, sudo already available)

1. `sudo -n i2cdetect -y 2` — confirm PMIC 0x4c / OV2680 0x10 powered now or still hidden.
2. `sudo -n cat /sys/bus/i2c/devices/i2c-OVTI2680:00/status 2>/dev/null; ls /sys/class/regulator* /sys/class/clk* 2>/dev/null` — confirm no rail/clk exists.
3. Check SCC/EC camera-power state: is `SCSS` readable anywhere (`/sys/firmware/acpi` objects, or the EC via `ec_sys`)? Look for a `power_state`/`SCSS`-style object.
4. If SCSS can't be set: prototype the userspace **gpio-regulator** for AVDD/DOVDD/DVDD + a **clk** for MCLK (19.2 MHz, MCLK port 0 per Windows scan) and force the media link + `v4l2-ctl -d /dev/video1 --stream-mmap` to get a real capture. That is the fastest path to proof-of-streaming.
5. Secondary: `sudo -n modprobe ov5648`, watch async probe for CAM1 (confirm rear cam physically present).

---

## Streaming test (day 15 — empirical closure)

Chased the last empirical lead before concluding. Two decisive results:

1. **The media link enable was failing only on a `media-ctl` flag-syntax bug.**
   media-ctl 1.26.1 wants flags as `[1]` (brackets):
   ```
   media-ctl -d /dev/media0 -l '"ov2680 2-0010":0 -> "ipu3-csi2 1":0 [1]'
   ```
   The `,1` (comma) separator I'd been using returned `Unable to parse link: EINVAL`.
   With the bracket syntax the link **enables cleanly** — topology now shows
   `ov2680 2-0010:0 [ENABLED]`. So the media topology itself is fine; the
   "link disabled" symptom was a tooling error, not firmware.

2. **The ov2680 has NO hardware MCLK provider, so it cannot generate frames.**
   - `/sys/class/clk` is absent; the only clk on the box is `i915 … rawclk rate:
     24000 kHz` (display). No `clk_intel`, no `fixed_clock`, no `gpio-24mhz`.
   - The ov2680 driver (`ov2680.c`) uses `devm_clk_get_optional(dev, "xvclk")` and
     reads a `clock-frequency` property (validated against 19.2/24 MHz). It probed
     (`sensor_revision id = 0x2680`) with **no** clock error → it found a
     *software* clock-frequency value but there is no physical clock to source the
     pixel clock from.
   - `ov2680_power_on()` calls `clk_prepare_enable(sensor->xvclk)`; when `xvclk`
     is NULL that is a no-op, so the sensor powers on, but the PLL cannot lock and
     no pixel clock is produced.
   - Result: opening `/dev/video1` returns `ENXIO` ("No such device") — the media
     node exists but the pipeline never enters streaming state.

**Conclusion from the test:** the camera is *enumerated and link-enabled* today;
the only remaining gap is a **hardware MCLK**. That MCLK is produced by the CRD
PMIC (TPS68470), whose output is gated by SCC-owned globals (`CL00`/`C0TP`/`SCSS`,
all External, written nowhere in firmware) and fed by SCC input power
(`Power0`/`Power1`). The SCC is a firmware controller with no PCI device, no MMIO
in `/proc/iomem` (`0x14001b000` register file is unreachable), no MEI client, and a
port-EC (`0x66/0x62`) with **no memory shadow** (`ec/ec0` exposes only `gpe/io/`,
`use_global_lock`). **Therefore the MCLK cannot be asserted from Linux.**

## Hard blocker on Option B (kernel MCLK clk provider)

The user picked "Kernel MCLK clk provider (Option B rebuild)". This was **ruled out** on
terms of first principles, before any rebuild:

- The ov2680 drives its MCLK pin via `clk_prepare_enable(sensor->xvclk)` in
  `ov2680_power_on()`. A software `clk-fixed-rate` makes `clk_prepare_enable` a **no-op** —
  it cannot toggle a physical pin. The ov2680 already found a software `clock-frequency`
  (that's why it probed with *zero* clock errors), so registering *another* software clock
  changes nothing: the PLL still can't lock without a real 24 MHz on the pin.
- That real clock is generated by the **CRD PMIC (TPS68470)** — SCC-owned, gated by
  `CL00/C0TP/SCSS` and powered by `Power0/Power1`. The SCC is unreachable from Linux.
- No other registered clock exists (only i915's display `rawclk`); no reachable board
  oscillator. EC shadow (`/sys/kernel/debug/ec/ec0/io`) exposes no Mclk/oscillator state.

**Conclusion:** no software/kernel module on this box can fabricate the physical MCLK.
Option B cannot make the camera stream. The camera needs the SCC-owned PMIC (or its data).

## 6. Open question for the bmad session

- **Architect (Winston):** option A (userspace gpio-regulator + clk, no kernel rebuild) vs B
  (CRD SSDTRM upstream support). Blast radius + reversibility on a chezmoi daily driver.
- **Dev (Amelia):** can we assert the SCC camera-power (`PowerEn`/`Power1`) from Linux at all,
  today, without a kernel module? That gate decides whether A is even reachable.

## 7. Reference map
- Windows firmware DSDT (decoded): `acpidump.win/win_tables/` (`DSDT_02c9dd.dat` + hand-disassembled CAM0/CAM1/PMIC byte ranges).
- Kernel driver: `linux-source-6.8.0/drivers/platform/x86/intel/int3472/{discrete.c,clk_and_regulator.c}`.
- SCC/EC GPIO bank: `reveng_skc_gpio_pins.md`.
- Live status: `agent_work/_driver-case-live-status.md`; history: `agent_work/day14.md`, `int3472-probe-failure-analysis.md`.

## 8. SCC driver obtained — reverse engineering in progress

The user provided the Windows **SkcController.sys** (TPS68470 SCC camera driver):
`agent_work/scc_data/SkcController.sys` (137064 B, PE32+ x86-64). Strings confirm:
- `SSTps68470` PMIC state machine: `SensorOn/SensorOff`, `SensorPowerOn/Off`,
  `MclkOutput`, `SetGpio`, `SetRegister/GetRegister`, `InitializeControlLogic`,
  `ResetControlLogic`.
- `SSCrdG2TiSensor` (the CRD sensor wrapper), `SSTps68470VoltageWF/UF` (voltage rails).

This is the source of truth for: (a) the SCC register-access mechanism
(MMIO BAR base `0x14001b????` vs port/SMI `0x66/0x62`), (b) the camera-on
register-write sequence (Power0/Power1/PowerEn/Mclk values), (c) the `Mclk`
enable bit + frequency, (d) the `C0P#/C0G#` pin map, (e) the CRD adoption
sequence (`CL00/C0TP/C0GP`).

Ghidra analysis running in background (agent 7404167a). Expected outcome: the exact
register values + access mechanism, which lets me (i) drive the PMIC from Linux
if the register block is reachable, or (ii) build a correct CRD `_CRS`/`_DSM` +
patched DSDT that exposes DOVDD/DVDD/AVDD/Mclk to the unmodified driver.
