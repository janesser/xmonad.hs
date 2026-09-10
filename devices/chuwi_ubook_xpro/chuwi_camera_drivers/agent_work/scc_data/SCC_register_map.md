# SkcController.sys — SCC extraction, consolidated (day15/16/17 + this session)

Target: `SkcController.sys` (Windows SCC camera driver, Chuwi UBook XPro).
Scope per `WINDOWS_HANDOVER_scc_extraction.md`: register-access mechanism,
camera-on write sequence (Power0/Power1/PowerEn/Mclk), Mclk enable bit + freq,
C0P#/C0G# pin map, CRD adoption.

Cross-reference: `day17.md` (confirmed SCC access is **I/O-port**, not MMIO;
Super-I/O register bank; per-register accessors; index seen = 0x1c,0x20,0x21,
0x22,0x2b,0x2c,0x2e,0x2f,0xd,0xf,0xb,0x10-0x1a). `day15_acpidump_windows.md`.

Ghidra 12.1.3 used. Binary: PE x86-64, 417 fns, 3528 syms. Key layout note:
the demangled `SSTps68470`/`SSCrdG2TiSensor` symbols sit in the `.text`
vtable/typeinfo region (0x140018000) which Ghidra sees as *data*; the real
method bodies live in 0x140001000-0x1400179c0. SCC methods are reached by
tracing xrefs to method-name strings.

---

## 1. Register-access mechanism  (deliverable #1)

**SCC register writes are I/O-port accesses (`outsb`/`outsl` to `%dx`), NOT
MMIO.** Confirmed by the `outsb`(D4)/`outsl`(D5)/`insb`(D0)/`insd`(D1) opcodes
present in `.text` (day17; `grep_io.py`), and by the per-register accessor
pattern (`mov` a global context into `%rcx`; check instance validity
`mov 0x2c(%rcx),%eax; test $0x1,%al`; set `%edx` = register index; call the
shared writer). `mov $0x494e434c,%edx` ('LNCI' little-endian) precedes a
power-off/de-init accessor.

### The low-level writer (disassembled this session)
`FUN_14000dfe4` (the shared SCC write routine, entry 0x14000dfe4) —
signature `FUN_14000dfe4(HAL_ctx, instance, buffer)`:

1. Zeroes a 0x30-byte buffer.
2. `FUN_140001924(instance, reg_index)` — context/value lookup (returns EAX).
3. Loads `uVar2 = instance[0x308]` and the (ushort) register index, builds a
   byte-encoded record in the buffer from the 32-bit value (byte count chosen
   by access-type param_4 = 1..4, i.e. 1/2/4-byte writes).
4. Calls the **runtime-injected HAL writer** by indirect call:
       `(*HAL_writer)[0x14001d7b0](HAL_ctx[0x14001df70],
                                   instance[0x308],
                                   <ptr at [0x14001d7b0+0x10]>,
                                   0x41808,     // flags/mode
                                   buffer)`.

### What is statically pinned vs not
- **Statically pinned:** register-index encoding (ushort index + 32-bit value),
  the `instance[0x308]` field passed as the HAL handle/base, the mode/flags
  value `0x41808`, the `*HAL` indirection at `0x14001d7b0`.
- **NOT statically pinned (runtime-injected HAL):** `0x14001d7b0` and
  `0x14001df70` are **zeroed in the image** (the HAL object + vtable are
  heap-allocated at load by the platform HAL injection; `FUN_14000a2b0` is the
  known heap-injector entry). Therefore the **exact I/O port (`%dx`) is not
  recoverable by static analysis** — it lives in the injected HAL body.

**Recover the exact port + Power0/Power1/PowerEn values by runtime inspection
(Per handover §6, the intended path):** break on the shared writer
`0x14000dfe4` / `0x14000da94` (or the `*HAL` indirect call at `0x14001d7b0`)
and read `%dx` (port) / `%edx` (index) / the ported data at the moment the
camera powers on. See `TODO_port_and_power_values.md` for the exact
breakpoint commands.

---

## 2. SCC register file (this session's main result)

Two register banks, both written via `FUN_14000da94(ctx,index,value,size)`
(write) / `FUN_14000d964(ctx,index,&buf,size)` (read):

| Bank | Index | Function (decompiled entry) | Field / note |
|------|-------|-----------------------------|--------------|
| MCLK enable | **0x0d** | `Tps68470Clock::SetHCLKAB` (0x14000a524) | bit 0 = enable (`|1` on, `&0xfe` off) |
| MCLK rate   | **0x0f** | same | bits 2–3 (`&0xc`) select frequency |
| Power rail A enable | **0x1a** | `SSTps68470VoltageUF::Initialize` (0x1400072f8) | written `0x8a` |
| Power rail B enable | **0x1c** | same | written `0x8a` |
| Indicator power | **0x28** | `IndicatorPowerOn`(0x14000b63c)/`Off`(0x14000b500) | on: bit2 (cam5)/bit6 (cam4); off: bit5 |
| Flash enable  | **0x2c** | `FlashInitialize` (0x14000aef8) | `param_1[0x8] & 0x3f` |
| Flash power   | **0x2d** | same | `param_1[0xc] & 0x3f` |
| Flash config  | **0x2e** | same | `param_1[0x10] & 0x3f` |
| Flash config  | **0x2f** | same | `param_1[0xe] & 0x3f` |
| Flash config  | **0x30** | same | `param_1[0x10] & 7` |
| VDDC / VCC   | **0x3d** | `SSTps68470VoltageUF::Initialize` | voltage from `param_1[8]` |
| VDDA          | **0x3e** | same | voltage from `param_1[0x10]` |
| VCC rail      | **0x3c** | `SSTps68470VoltageWF::Initialize` (0x1400074e4) | voltage from `param_1[0xe]` |
| VCC rail      | **0x3f** | same | voltage from `param_1[0xc]` |
| VCC rail      | **0x40** | same | voltage from `param_1[0x10]` |
| VCC rail      | **0x41** | same | voltage from `param_1[0x10]` |
| VPP           | **0x42** | same | `(param_1[8]-900)/25` |
| VD control    | **0x45** | `SetVDCtl` (0x140007f8c) | bit 0 = enable (from `param_3`) |
| VA control    | **0x47** | `SetVACtl` | bit 0 = enable (from `param_3`) |
| VCM control   | **0x44** | `SetVCMCtl` | — |
| IO control    | **0x43** | `IoActive` | — |

Voltage values are **computed from a config struct at runtime** (linear
decode: `(V - 875)/17.8` clamped to `&0x7f`; VPP via `(V-900)/25`), so the
exact byte for a given rail depends on the board config — **not a fixed
constant**. That is why the handover's "fixed Power0/Power1/PowerEn/Mclk
values" are config-derived for the voltage rails; only MCLK and the power-rail
enable (0x8a) are fixed.

### SCC power-rail enable (Power0/Power1)
The two SCC power rails are enabled by writing **`0x8a`** to registers **0x1a**
and **0x1c** (`0x8a = 0b1000_1010`, bit 7 = power-channel enable). These are the
fixed values the handover wants for "Power0/Power1/PowerEn". Confirmed in
`SSTps68470VoltageUF::Initialize` (0x1400072f8): `0x1a,0x8a` then `0x1c,0x8a`.

---

## 3. Mclk enable bit + frequency  (deliverable #3 — resolved)

From `Tps68470Clock::SetHCLKAB` (`FUN_14000a524`, entry 0x14000a524), the
decompiled logic:

```
tmp = SCC_read(0x0d)
write(0x0d, tmp & 0xfe)          // clear enable
tmp = SCC_read(0x0f)
bVar3 = (param_1[0x16] ^ tmp) & 3 ^ tmp      // from config
rate  = param_1[0x17] & ...                    // from config
write(0x0f, (rate << 2 ^ bVar3) & 0xc ^ bVar3) // bits 2-3 select freq
write(0x0d, (tmp & 0xfe) | 1)                  // re-set enable
```

- **Enable bit: bit 0 of register 0x0D.** Writing 1 enables MCLK, writing 0
  (via `&0xfe`) disables it.
- **Frequency select: bits 2–3 (mask 0x0c) of register 0x0F.** The 2-bit value
  selects one of 4 MCLK rates; the exact value is derived from the board config
  (`param_1[0x16]`/`param_1[0x17]`), not a hard constant. (The 24 MHz vs
  19.2 MHz choice is whichever 2-bit field the board config installs — confirm
  against the config blob at runtime.)
- `CrdG2TiClock::SetHCLKAB` (`FUN_14000a3d0`, entry 0x14000a3d0) is the other
  sensor's clock: reference-counted (0x20), and on the zero-transition it also
  calls `0x14000a524`.

---

## 4. Camera-on register write sequence  (deliverable #2 — resolved)

### Front camera = `SSCrdG2TiSensor::SensorPowerOn` (`FUN_140006e00`, entry 0x140006e00)
`param_3 == 5` selects the OV2680 front sensor. The ordered write sequence:

1. **`SSTps68470VoltageWF::Initialize`** (`0x1400074e4`) — 5 voltage rails, in
   this order:
     `0x41`, `0x40`, `0x42`, `0x3c`, `0x3f`   (values = config-derived voltage decodes)
2. **`SetVACtl`** (`0x14000da94` reg `0x47`) — set bit 0 (enable) when
   `param_3 != 0` (i.e. `== 5`).
3. **`SetVCMCtl`** (`reg 0x44`) — VCM.
4. **`IoActive`** (`reg 0x43`) — drive IO active.

So the canonical front-camera power-on register sequence is:
**0x41 → 0x40 → 0x42 → 0x3c → 0x3f → 0x47(bit0) → 0x44 → 0x43**, plus the
two fixed power-rail enables **0x1a=0x8a, 0x1c=0x8a** and the MCLK enable on
**0x0d bit0 / 0x0f bits2-3** (from `Tps68470Clock::SetHCLKAB`).

### Rear camera (for completeness)
`CrdGTiSensor::SensorPowerOn` (`0x140018e70`, real body 0x140007734) and
`CrdG2TiSensor::SensorPowerOn` (`0x140018e70`/`0x140007ed3`) are the other two;
same four-step pattern through the voltage-set helpers.

---

## 5. C0P# / C0G# pin map  (deliverable #4 — partial)

`SetGpio` dispatcher (`FUN_140006fd0`, entry 0x140006fd0) routes
`<sensor>::SetGpio` via the virtual table (`mov rax,[rcx]; call qword ptr
[rax + offset]`), and `InitializeControlLogic` (`FUN_140005ef8`) routes
`<sensor>::InitializeControlLogic` the same way — but **both dispatch to
heap-allocated derived objects**, so the concrete GPIO/CRD-logic bodies are not
in the image statically.

`SetGpio` takes `(this, int gpio_num, bool value)` — i.e. it selects a GPIO by
index and sets it; the C0P#/C0G# ↔ gpio_num mapping lives in the runtime GPIO
table. **To get the exact pin map**, break in `SetGpio`/`SetGpio`-derived body
at camera init and read which `gpio_num` maps to C0P# and C0G#.

---

## 6. CRD adoption sequence  (deliverable #5 — partial)

`InitializeControlLogic` (`FUN_140005ef8`) and each `<sensor>::
InitializeControlLogic` are reached through the same vtable dispatch as `SetGpio`;
the real implementation (the CL00/C0TP/C0GP ordering the handover calls "CRD
adoption") is in the runtime-constructed sensor object, not statically present.
Same runtime-inspection path as §5.

---

## 7. Still requires runtime inspection (per handover §6)
- Exact I/O port (`%dx`) + select/data ports — in the injected HAL (`0x14001d7b0`/`0x14001df70`).
- Exact Power0/Power1/PowerEn byte values for the rear camera — config-derived; only the front-rail enable (`0x8a` on 0x1a/0x1c) is fixed.
- Exact MCLK 24 MHz vs 19.2 MHz value — config field `param_1[0x16]/[0x17]`.
- C0P#/C0G# ↔ gpio_num and CL00/C0TP/C0GP order — runtime sensor object.

See `TODO_port_and_power_values.md`.

---

### Reproduce
- `.text` extract + opcode scan: `scan_imm.py`, `scan_vtable.py`, `extract.py`, `grep_io.py` in this dir.
- SCC register map: Ghidra decompile of 0x14000a524, 0x14000a3d0, 0x1400072f8,
  0x1400074e4, 0x14000aef8, 0x14000b500, 0x14000b63c, 0x140007f8c, 0x140006e00,
  0x140006fd0, 0x140005ef8, 0x14000dfe4, 0x14000da94.
