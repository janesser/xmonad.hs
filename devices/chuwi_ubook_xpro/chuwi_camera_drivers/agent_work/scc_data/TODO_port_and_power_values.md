# TODO — remaining SCC items (runtime inspection, per handover §6)

All of the following live in code/objects that Ghidra can't see statically
(runtime-injected HAL + heap-constructed sensor objects). Recover them with
kernel-mode WinDbg on the driver (this is the correct layer for a .sys), exactly
as the handover §6 prescribes.

## A. Exact I/O port(s) + Power0/Power1/PowerEn bytes (deliverable #1 + #2)
The shared SCC writer is `0x14000dfe4` (builds the byte record) and
`0x14000da94` (the register write). The port is passed through the injected HAL:
`(*HAL)[0x14001d7b0](ctx[0x14001df70], instance[0x308], …, 0x41808, buffer)`.

WinDbg commands (load `SkcController.sys` + symbols):
```
bp SkcController!0x14000dfe4      // hit on every SCC register write
bu SkcController!0x14000da94      // hardware bp, resolves symbols
bp SkcController!0x14001d7b0      // the *HAL indirect call (port actually issued here)
```
At the hit, inspect:
- `%dx`  — the I/O port number
- `%edx` — the SCC register index (we already know these: 0x1a/0x1c=0x8a, 0x41/
  0x40/0x42/0x3c/0x3f, 0x47, 0x44, 0x43, 0xd, 0xf, 0x2c-0x30, 0x28, …)
- the ported data byte — the Power0/Power1/PowerEn/Mclk value
- `dq @rcx` — the HAL context (0x14001df70) and vtable (0x14001d7b0) so you can
  pin the HAL body that owns the real port.

We already have the fixed values:
- **Power0/Power1 enable = 0x8a written to SCC registers 0x1a and 0x1c**
  (`SSTps68470VoltageUF::Initialize`, 0x1400072f8).
- **MCLK = reg 0x0d bit0 (enable) + reg 0x0f bits2-3 (rate)**; the 24 vs 19.2 MHz
  value is `param_1[0x16]/param_1[0x17]` from the board config — read it at the
  `SetHCLKAB` (0x14000a524) hit.

## B. C0P# / C0G# pin map (deliverable #4)
`SetGpio` = `0x140006fd0` (`SetGpio(this, int gpio_num, bool value)`). Break on
the derived `SetGpio` body at camera init and read the table that maps each
`gpio_num` to the physical pin (C0P#, C0G#, flash, …). The mapping table is in
the runtime sensor object.

## C. CRD adoption / CL00-C0TP-C0GP sequence (deliverable #5)
`InitializeControlLogic` = `0x140005ef8` (vtable dispatch to the runtime sensor).
Trace the call to the derived `InitializeControlLogic` and log the register
indices/values in execution order. Same WinDbg approach as (A).

## Quick static confirmation without a debugger
If only static RE is possible, the SCC register map in `SCC_register_map.md` is
complete enough to drive a port-emulation replay: write 0x8a to 0x1a/0x1c,
power the 5 voltage rails (0x41/0x40/0x42/0x3c/0x3f) with config-derived
values, set 0x47/0x44/0x43, then enable MCLK (0x0d bit0, 0x0f bits2-3). The
only missing constants are the exact port number and the config-derived MCLK
rate.
