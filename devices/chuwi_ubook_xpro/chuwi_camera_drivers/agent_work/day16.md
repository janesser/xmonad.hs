# Day 16 — SCC register-access investigation: where we stand + Windows extraction playbook

**Goal:** extract from `SkcController.sys` (Windows SCC camera driver, TPS68470/CRD PMIC) the exact
SCC register-access mechanism, camera-on register-write sequence, Mclk enable bit/frequency,
`C0P#/C0G#` pin map, and CRD adoption sequence (`CL00`/`C0TP`/`C0GP`). This data completes the
CRD-visibility patch (Option B on the int3472-discrete driver, see `day15_acpidump_windows.md`).

**Status: static RE partially done; the crux (SetRegister/GetRegister live) is still unextracted.**
Pivoted to a live-Windows extraction recipe because Linux-side tooling can't decode the SCC methods.

---

## 1. Critical new discovery: SCC is reached via the Windows "Resource Hub", NOT a mapped PCI BAR

- No SCC PCI device exists on this box (`lspci -v` shows no SCC/SuperIO/SignalProcessing/Platform class
  that owns a BAR; only MEI `00:16.0`, the CSI-2 host `00:14.3`, IOVMM `00:05.0`, ISA bridge `00:1f.0`).
- No Linux SCC / Resource-Hub / Super-I/O driver exists in `linux-source-6.8.0`.
- `/sys`, `/sys/kernel/debug/*` (including EC), `/sys/bus/lpc`, `/sys/bus/lpss` — **all empty** for SCC.
- The driver embeds the device names `\\Device\\RESOURCE_HUB` and `\\DosDevices\\RESOURCE_HUB`
  (found via `strings`). **So `SkcController.sys` does its register access by opening the
  `RESOURCE_HUB` device and issuing IoControl calls to it** — the Resource Hub is a Windows driver that
  hands out MMIO access to platform resources (PMICs/SCC). The SCC register block itself is a
  runtime-mapped MMIO region, not a static BAR.

This is the single most important finding for reachability: on Linux there is **no** Resource Hub
equivalent and **no** SCC device, so the SCC cannot be driven from Linux without a custom driver.
Windows is the only way to read the register map + values out of the running driver.

---

## 2. Static findings that survived (do not lose these)

### 2.1 SCC class variants (from `strings`, by offset)
`SSTps68470` (base, `s_tps68470::SSTps68470::SetRegister`), then platform variants:
`SSCrdG2TiSensor`, `CrdGTiSensor`, `CrdG2TiSensor`, `CrdG2TiQuantaGpio`, `up6641::CrdGUpiSensor`,
`CrdG2UpiSensor`, `CrdGUpiGpio`, `CrdG2UpiGpio`. Plus `GpioOper`, `SetGpio`, `MclkOutput`,
`InitializeControlLogic`, `ResetControlLogic`, `DEVICE_CONTEXT`. So one driver covers TPS68470 **and**
up6641 PMICs across multiple Chuwi/CRD sensor variants.

### 2.2 SCC register-name → offset map (from `reveng_skc_gpio_pins.md`)
Register block base `0x14001b000`; each register is 0x10-aligned:

| Register      | Offset (base+off) | Notes            |
|---------------|-------------------|------------------|
| Reset         | 0x14001b1d0       |                  |
| Enable / Strobe | 0x14001b1e0     |                  |
| Torch / Flash | 0x14001b1f0       |                  |
| LedRear / LedFront | 0x14001b200  |                  |
| Power0        | 0x14001b210       | camera rail 0    |
| Power1        | 0x14001b220       | rail 1 / Standby |
| WriteProtect  | 0x14001b230       |                  |
| PowerEn / Mclk| 0x14001b240       | main enable + Mclk |
| PrivateLED    | 0x14001b250       |                  |
| Reserved      | 0x14001b260       |                  |
| DEVICE_CONTEXT| 0x14001b270       | shadow           |

### 2.3 The base `0x14001b????` is INSIDE the driver's own `.rdata` (big clue)
PE layout (`objdump -h`):
```
0 .text         00019030  VMA 140001000  File 00000400
1 .rdata        00001a7c  VMA 14001b000  File 00019600
2 .data         00000200  VMA 14001d000  File 0001b200
3 .pdata        00001098  VMA 14001f000  File 0001b400
4 PAGE          0000189b  VMA 140021000  File 0001c600
5 INIT          000005f8  VMA 140023000  File 0001e000
6 .rsrc         00000370  VMA 140024000  File 0001e600
```
`0x14001b000` is the **start of `.rdata`** — i.e. the "register base" the strings/analysis report is a
*label* pointing into the driver's read-only data, **not** an MMIO BAR. The real SCC register block
base is obtained at runtime (through the Resource Hub handle) and added to the offsets above. The two
absolute pointers at the head of this table (`0x140017654`, `0x140017670`) are function pointers; the
trailing entries (`0x235be`, `0x235aa`, `0x23598`, `0x235d4`, `0x23378`, `0x23390`, … in the
`0x233xx/0x234xx/0x235xx` range) look like **register/field offsets** indexed by that table — a strong
candidate for the SCC field-offset table. Raw bytes of that table:
```
000000 54 76 01 40 01 00 00 00 70 76 01 40 01 00 00 00
000010 be 35 02 00 00 00 00 00 aa 35 02 00 00 00 00 00
000020 98 35 02 00 00 00 00 00 d4 35 02 00 00 00 00 00
000030 00 00 00 00 00 00 00 00 78 33 02 00 00 00 00 00
000040 90 33 02 00 00 00 00 00 aa 33 02 00 00 00 00 00
000050 c2 33 02 00 00 00 00 00 d6 33 02 00 00 00 00 00
000060 f0 33 02 00 00 00 00 00 fa 33 02 00 00 00 00 00
000070 06 34 02 00 00 00 00 00 18 34 02 00 00 00 00 00
000080 34 34 02 00 00 00 00 00 48 34 02 00 00 00 00 00
000090 5c 34 02 00 00 00 00 00 6a 34 02 00 00 00 00 00
0000a0 60 33 02 00 00 00 00 00 9e 34 02 00 00 00 00 00
0000b0 c2 34 02 00 00 00 00 00 d0 34 02 00 00 00 00 00
0000c0 e0 34 02 00 00 00 00 00 f8 34 02 00 00 00 00 00
0000d0 06 35 02 00 00 00 00 00 24 35 02 00 00 00 00 00
0000e0 30 35 02 00 00 00 00 00 44 35 02 00 00 00 00 00
0000f0 56 35 02 00 00 00 00 00 7c 35 02 00 00 00 00 00
000100 40 33 02 00 00 00 00 00 28 33 02 00 00 00 00 00
000110 14 33 02 00 00 00 00 00 82 34 02 00 00 00 00 00
000120 08 33 02 00 00 00 00 00 00 00 00 00 00 00 00 00
000130 00 00 00 00 3a 4b 40 56 00 00 00 00 02 00 00 00
000140 a2 00 00 00 70 ba 01 00 70 a0 01 00 00 00 00 00
```

### 2.3 tooling situation on THIS box (why we go to Windows)
- Ghidra analysis of `SkcController.sys`: **function detection dies at `memset` (0x1400179c0)**; the
  SCC methods (0x140017ff0–0x14001a1ff) are never detected. `ghidra action=functions` returns 384
  functions but none in the SCC region; `ghidra info` claims 417 (count includes symbol stubs).
  `ghidra action=decompile` (by name or address) and `ghidra action=disassemble` return **empty**.
- No `capstone`/`pefile` on the box; `/snap/ghidra/47/bin/python3` missing. Only usable Ghidra actions
  return `strings`/`info`/`data` (data action only matches strings).
- `objdump` **is** available (can disassemble raw `.text`/`.rdata` blobs manually if needed), but
  Ghidra's proper symbol/name resolution is what we need for the SCC methods.

**Conclusion:** static RE on Linux can get us the register *names/offsets* and the class variants, but
not the runtime values or the access mechanism. Go to a running Windows host.

---

## 3. Windows extraction playbook (next actionable step)

Run on a Windows host where `SkcController.sys` is loaded and the camera power sequence runs
(e.g. a desktop/laptop running the same SCC driver, or a VM image with it). Goal is to capture the
register values written during camera power-on and how the base `0x14001b????` is obtained.

### 3a. Confirm the driver + locate its module
- `driverquery /v` → confirm `SkcController` loaded, note its image path.
- Open **Process Explorer** → the `SkcController` process/module → right-click the module to see the
  base address (typically `0x140000000` for a driver). Note the module base; `.text` starts at
  base+`0x1000`, `.rdata` at base+`0x1b000` (that's where the `0x14001b000` label lives).

### 3b. Get the SCC register-block base + access mechanism
- **WinDbg (kernel mode, on the target host or via a VM serial/1394 connection):**
  - Break, then `ln *SkcController!*SetRegister*` and `ln *SkcController!*GetRegister*` to locate the
    methods by name.
  - Set a breakpoint on `SetRegister` (`bp <addr>`). Watch the value the function writes to the
    `0x14001b????` base — read the pointer it dereferences (`dqs <ptr>` / `dps`). This reveals the
    **access mechanism** (MMIO read/write vs port `0x66/0x62` vs SMI) and the **runtime base address**.
  - If it goes through `RESOURCE_HUB`: `ln *resourhub!*<ioctrl>` and inspect the IoControl code and
    the MMIO base it returns/mmaps.
- **Fallback (VMMap):** attach VMMap, confirm the `SkcController` mapping and any MMIO region the
  Resource Hub hands out; record its physical address.

### 3c. Capture the camera-on register-write sequence
- Set `bp SetRegister` and `ed rax 1; g` (or log each `reg=off value=(addr)`).
- **Trigger the camera power-on** so `SensorOn` runs: enumerate/activate the camera device IRP, or
  run whatever app/service powers the SCC camera. Every `SetRegister` call during power-on is the
  sequence we need:
  - `Power0` value, `Power1` value, `PowerEn` value, `Mclk` value, `Enable` value, `Reset` value.
- Record the **exact 32-bit values** written to each of the offsets from §2.2. Those are the values
  to reproduce (via int3472-discrete + a patched DSDT, once the access path from Linux is confirmed).

### 3d. Decode Mclk
- From the `Mclk` register write (§2.2 offset `0x14001b240`, shares with `PowerEn`):
  - Identify the enable bit and the frequency-select field (the driver likely sets a frequency via a
    multi-bit field; `MclkOutput` method will show which of 24 MHz / 19.2 MHz / etc.).
  - Note whether Mclk is an enable bit in a power register vs a dedicated register bit.

### 3e. C0P# / C0G# pin map
- Break on `SetGpio` / inspect `InitializeControlLogic` and `ResetControlLogic` for the GPIO table that
  maps SCC logical lines to physical pins. Capture which lines are `C0P#` (power enable) and `C0G#`
  (general GPIO), and the register/field each controls. Cross-reference with the CRD `_CRS`
  `PINR(C0P#, C0G#)` from the DSDT.

### 3f. CRD adoption sequence (CL00 / C0TP / C0GP)
- These are CRD SSDTRM globals set only by Windows (see DSDT `External`s at lines 1041–1066;
  `day15_acpidump_windows.md`). Find the method in `SkcController.sys` that sets them — likely in
  `InitializeControlLogic`/`ResetControlLogic`/a `_DSM` handler on the CRD PMIC GUID.
- Capture the **order** and the **conditions** (the `CL00 && (C0TP==One)` gate, then `C0GP=N`):
  what writes `CL00`, what then writes `C0TP`, and that `C0GP` is asserted (cleared) to open the gate.
  This is exactly the adoption sequence the kernel's classic `int3472-discrete` path bypasses.

### 3g. Dump the driver for later static disassembly
- If direct kernel debugging is awkward, dump the module (`!dumpbin`/`SysInfo`/`VMMap` save) and hand
  the dumped `SkcController.sys` + the captured values back to this box for objdump/Ghidra decoding
  of the exact instructions.

---

## 4. Why this matters for the fix (mapping to day15)

- If 3b shows the SCC register block is **MMIO and visible from Linux** (e.g. a region under the ISA
  bridge or an ACPI memory resource), then reproducing the SCC enable writes from Linux may be
  possible — but the Resource Hub finding (§1) says this is unlikely on Linux (no SCC PCI device, no
  Linux Resource Hub driver).
- Either way, the **values** from 3c/3d and the **adoption sequence** from 3f are what complete the
  CRD-visibility patch: an `int3472-discrete` CRD `_DSM`/`_CRS` parser (reading GPIOs from `_DSM`
  `GPPI` fn2+ since `_CRS` carries no GPIOs) + a patched DSDT forcing `CL00`/`C0TP`, `C0GP=N`, wiring
  `C0P#/C0G#`. The SCC driver is the source of truth for those values and the sequence; the Linux side
  can only *reproduce* them if a Linux access path exists.

## 5. Open questions for the Windows run
- Does the SCC register access go through `RESOURCE_HUB` IoControl (likely) or direct MMIO/ports?
- What is the exact runtime base address for `0x14001b????`?
- Are there register values that differ between `SSTps68470` vs `up6641` variants (we have both in
  the driver)?
- Which physical pins are `C0P#`/`C0G#`, and does the driver's GPIO table match the DSDT `PINR`?
