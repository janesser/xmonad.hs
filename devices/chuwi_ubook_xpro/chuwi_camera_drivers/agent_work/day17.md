# Day 17 — SkcController.sys static RE (objdump): SCC access is port-I/O via a Super-I/O register bank

**Goal:** extract the SCC register-access mechanism + camera-on register values from
`SkcController.sys` (already RE'd in day15/day16). The limited Ghidra tool here is insufficient
(`disassemble`/`decompile`/`symbols` all return empty; function detection dies on the SCC region),
so I used `objdump` (via `objcopy`→ELF) to bypass Ghidra's function-detection problem.

**Box tooling:** no capstone/pefile; `/snap/ghidra/47/bin/python3` missing. `objdump`/`objcopy`
available. Ghidra actions that work: `info` (417 fns, 242 syms), `strings`, `data`(strings only),
`functions`(384, none in SCC region). `decompile`/`disassemble`/`symbols` return empty.

## How to disassemble a .sys on this box (worked around Ghidra)
```
dd if=agent_work/scc_data/SkcController.sys bs=1 skip=$((0x1c600)) count=$((0x189b)) of=/tmp/page.bin  # .PAGE
objcopy -I binary -O elf64-x86-64 /tmp/page.bin /tmp/page.o
objdump -d -j .data --show-raw-insn /tmp/page.o          # note: addresses are objcopy-relative, base 0x280000000
```
Caution: `objdump -d` disassembles *all* of a section including data — `outsb`/`outsl` that appear
in the middle of ASCII strings (e.g. `50 6c 61 74 66 6f 72 6d` = "form…") are **false positives**.
Only treat a port instr as real when it sits in a function with a proper prologue (`sub …,%rsp`).

## What the SCC driver does (confirmed)
1. **Port I/O, not MMIO.** SCC register writes go through `outsb`/`outsl` to `%dx`. The SCC register
   block base `0x14001b????` seen in day16 §2.3 was just the `.rdata` base (a label), NOT an MMIO BAR.
2. **Super-I/O register-bank access.** Each SCC register has a tiny accessor fn that:
   - loads a **global instance/context pointer** (static slot, e.g. `lea 0x16e0(%rip),%r9`),
   - checks instance validity (`mov 0x2c(%rcx),%eax; test $0x1,%al`),
   - sets **edx = register index** (values seen: `0x2b,0x2c,0x2e,0x1c,0x22,0x20,0x21,0xb,0xf,0x10,
     0x12…0x1a`),
   - then calls the shared write routine in `.text`.
3. `mov $0x494e434c,%edx` (='LNCI') precedes one accessor that also zeroes a global flag — likely the
   power-off / de-init path.

## Still NOT extracted by objdump (needs real function boundaries → Ghidra or WinDbg)
- The concrete **SCC port number** (`%dx`) and select/data ports.
- The **camera-on register VALUES** (Power0/Power1/PowerEn/Mclk) written by `SensorOn`.
- `C0P#/C0G#` pin map; `CL00`/`C0TP`/`C0GP` adoption order.

## Follow-up (this session — Ghidra 12.1.3 on a capable box)
- **Register-access mechanism**: SCC writes go through the shared writer
  `0x14000dfe4`/`0x14000da94`, which encodes the ushort index + 32-bit value
  and calls a **runtime-injected HAL** via `(*HAL)[0x14001d7b0](ctx[0x14001df70],
  instance[0x308],…,0x41808,buffer)` — both addresses are **zeroed in the image**
  (heap-allocated). So the exact `%dx` port is NOT statically pinned (needs the
  runtime HAL — see `SCC_register_map.md` §1).
- **Full SCC register map extracted** (`SCC_register_map.md`): MCLK = reg 0x0d
  (bit0 enable) + 0x0f (bits2-3 rate); power-rail enable = **0x8a → 0x1a/0x1c**;
  flash 0x2c-0x30; indicator 0x28; voltage rails 0x3c-0x47 (config-derived values).
- **Camera-on sequence resolved**: front cam (OV2680, `param_3==5`,
  `SSCrdG2TiSensor::SensorPowerOn` 0x140006e00): 0x41→0x40→0x42→0x3c→0x3f→0x47→
  0x44→0x43, plus power enables (0x1a/0x1c=0x8a) and MCLK enable.
- C0P#/C0G# map + CL00/C0TP/C0GP order remain in the heap-constructed sensor
  object → runtime inspection (`TODO_port_and_power_values.md`).

## Bottom line / recommendation
objdump confirms *architecture* (Super-I/O port-I/O + per-register accessors) but is too
error-prone to nail the exact port + values. Two clean paths remain — see the response to the user:
- **Ghidra on Windows** (via pi-agent on Windows, or their machine): proper function detection +
  decompile of `SetRegister`/`SensorOn` → cleanest, gets everything at once.
- **Windows kernel WinDbg**: bp the shared write routine, inspect %dx/%edx/ported values at the
  moment the camera powers on.
(Neither is "process digging" — both are kernel-mode, the correct layer for a kernel driver.)
