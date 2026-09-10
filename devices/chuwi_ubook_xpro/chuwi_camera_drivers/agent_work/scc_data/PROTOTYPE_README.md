# scc_camera_proto.c — prototype notes

**Prototype only.** A Linux kernel module that emulates the Windows `SkcController.sys`
camera power-on by writing the SCC Super-I/O register bank over I/O ports. It encodes the
register map + power-on sequence extracted by the Windows Ghidra run
(`SCC_register_map.md`).

## What it does
`SSTps68470VoltageWF::Initialize` (5 rails) → `SetVACtl` → `SetVCMCtl` → `IoActive` → MCLK
enable, plus the fixed `0x1a`/`0x1c` = `0x8a` rail enables. Register indices, the `0x8a`
enable writes, the MCLK enable/rate registers and the write order are **statically known**.

## Runtime gaps that MUST be filled before it powers a camera
1. **I/O port** (`scc_port`) — the register writer calls a *runtime-injected HAL*
   (`(*HAL)[0x14001d7b0]`); the port lives in that HAL body, not in the image.
2. **Byte-record encoding** (`scc_width`) — SkcController encodes each 32-bit value as a
   1/2/4-byte record; we default to `outb` (width 1). Confirm against the HAL.
3. **Voltage-rail values** (`scc_rail_value[]`) — decoded at runtime from a config struct
   `(V-875)/17.8` clamped to `&0x7f` (VPP `(V-900)/25`); board-config dependent, not fixed.
4. **MCLK rate** — bits 2–3 of reg `0x0f` come from the board config (`param_1[0x16/0x17]`).

## Fill the gaps with kernel WinDbg (see `TODO_port_and_power_values.md`)
```
bu SkcController!0x14000dfe4    // shared writer
bu SkcController!0x14000da94    // the register write
bp SkcController!0x14001d7b0    // the *HAL indirect call (port issued here)
```
At a hit during camera power-on: read `%dx` (port), `%edx` (register index), the ported data
byte (the rail/MCLK value), and `dq @rcx` to pin the HAL body that owns the real port.

## Build (kernel tree / module path)
```
# in a kernel tree with the module dir:
make M=/path/to/agent_work/scc_data modules
# or standalone:
make -C /usr/src/kernel-<ver> M=$(pwd) modules
```
Loading needs root + a loadable-module path (see `PORT`/secure-boot caveats in the module header).
This is a **daily driver** — do not `make install` until the four gaps above are filled and the
camera is verified.
