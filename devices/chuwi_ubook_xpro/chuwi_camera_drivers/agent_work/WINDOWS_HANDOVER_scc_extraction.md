# HANDOVER — pi-agent on Windows: extract SCC register data from `SkcController.sys`

**Task:** Reverse-engineer the Windows SCC camera driver `SkcController.sys` (Intel "Control
Logic") with **Ghidra** and extract the data needed to complete the Linux camera fix on the
Chuwi UBook XPro. This is the single missing piece (see `day15_acpidump_windows.md`,
`day16.md`, `day17.md`).

**Why Windows + Ghidra:** the SCC is a *kernel* driver that powers the camera on Windows but is
unpowered on Linux, so the camera is dead on Linux. We already know the **architecture**
(`day17.md`) — port I/O to a Super-I/O register bank, per-register accessors — but the exact
**port number** and **camera-on register values** need function-level analysis (Ghidra) or live
kernel debugging (WinDbg). This handover is for the Ghidra path.

---

## 1. Objective — deliverables (what the Linux side needs)

From `SkcController.sys` you must produce, in plain text/Markdown:

1. **Register-access mechanism + port numbers.**
   - The exact I/O **port(s)** used (`%dx`) and the **select/data** port sequence for the
     Super-I/O register bank.
   - Whether writes are byte (`outsb`) or dword (`outsl`), and any select-port/index handshake.
2. **Camera-on register write sequence.** For each of `Power0`, `Power1`, `PowerEn`, `Mclk`
   (and `Enable`/`Reset` if present): the **exact 32/16-bit value** written and the **order** of
   writes during `SensorOn` / `SensorPowerOn`.
3. **Mclk enable bit + frequency.** From `MclkOutput`: which bit enables Mclk, and the selected
   frequency (24 MHz vs 19.2 MHz vs …) — the field in the Mclk register that sets the rate.
4. **`C0P#` / `C0G#` pin map.** From `SetGpio` + `InitializeControlLogic`: which SCC logical lines
   drive `C0P#` (power enable) and `C0G#` (general GPIO), and which register/bit each controls.
5. **CRD adoption sequence.** The order + conditions of `CL00`, `C0TP`, `C0GP` (the `CL00 &&
   (C0TP==One)` gate that makes the PMIC visible — see `day15`/`day16`). Wherever it lives
   (`InitializeControlLogic`, `ResetControlLogic`, or a CRD `_DSM` handler).

If a value can't be pinned down statically, say so explicitly and give the closest evidence — do
not guess.

---

## 2. Context (already known — don't rediscover)

- **Host:** Chuwi UBook XPro. Camera = OV2680 (front, I2C @0x10) + OV5648 (back, @0x36); PMIC =
  TPS68470 (`INT3472`, "PMIC-CRDG"). Same building blocks as Lenovo Miix 510 (djscally
  `miix-510-cameras`, classic INT3472 path — *not* our device).
- **Our device uses the SCC in the power path:** the SCC asserts `SCSS` (PMIC visibility) and drives
  `Power0/Power1/PowerEn+Mclk`. Gate is `CL00 && (C0TP==One)`, set only by Windows. On Linux these
  stay 0 → PMIC hidden (`_STA=0`) → kernel `int3472-discrete` can't even probe it.
- **No SCC PCI device** on Linux; SCC register bank is **port-mapped**, not MMIO.
- **Confirmed static facts (`day17`):** writes go via `outsb`/`outsl` to `%dx`; each register has a
  small accessor fn that loads a global instance pointer, checks `[rcx+0x2c]` bit 0, sets
  **`edx` = register index** (`0x2b, 0x2c, 0x2e, 0x1c, 0x22, 0x20, 0x21, 0xb, 0xf, 0x10…0x1a`),
  then calls a shared write routine. `mov $0x494e434c,%edx` ('LNCI') precedes one de-init path.
  (These objdump facts were from a constrained box; treat the *values* as unconfirmed until
  Ghidra confirms them.)
- **Symbol hints (from `strings` on the box):** classes `SSTps68470`, `CrdGTiSensor`,
  `CrdG2TiSensor`, `CrdGUpiSensor`, `CrdG2UpiSensor`, `up6641`; methods `SetRegister`, `GetRegister`,
  `SensorOn`, `SensorOff`, `SensorPowerOn`, `SensorPowerOff`, `MclkOutput`, `SetGpio`,
  `InitializeControlLogic`, `ResetControlLogic`. Register names present: `Reset`, `Enable`,
  `Power0`, `Power1`, `PowerEn`, `Mclk`, `DEVICE_CONTEXT`.

---

## 3. Prerequisites on the Windows host

- **Ghidra** installed (11.x or newer recommended). If not installed: download the GHZ/zip from
  https://ghidra-sre.org/, or `choco install ghidra`. Ensure **Java 17+** is on `JAVA_HOME`
  (Ghidra needs it; `analyzeHeadless` will complain otherwise).
- **The driver binary.** Get it from the running system:
  - If a Windows machine/VM with the driver loaded: copy
    `C:\WINDOWS\system32\drivers\SkcController.sys` (admin). Or, from the captured environment, the
    binary already exists at `agent_work/scc_data/SkcController.sys` on the pi box — but use a
    freshly captured copy from the live Windows system if possible.
  - `driverquery` already confirmed it loads as **`SkcController` = "Intel(R) Control Logic"**,
    Kernel, Running.
- Work in a clean folder, e.g. `C:\scc-re\`. Put the `.sys` there.

---

## 4. Ghidra headless workflow (the core)

Ghidra has **correct function detection + decompilation** (unlike the constrained tool on the pi
box), so this is the reliable path. Run headless — no GUI needed.

### 4.1 Analyze + decompile in one command

From `C:\scc-re\` (adjust `ghidraHome` and the paths):

```bat
"%ghidraHome%\support\analyzeHeadless" ^
  C:\scc-re\project sccproject ^
  -import C:\scc-re\SkcController.sys ^
  -deleteProject ^
  -postScript C:\scc-re\decompile_scc.py ^
  -appendLog
```

- This creates project `C:\scc-re\project\sccproject.gpr`, fully analyzes the binary, then runs the
  postScript, appending everything to `C:\scc-re\analyzeHeadless.log`.
- First run analyzes (may take a few minutes). If it fails on Java, set `JAVA_HOME` first:
  `set JAVA_HOME=C:\path\to\jdk17` and re-run.

### 4.2 The postScript — `C:\scc-re\decompile_scc.py`

This opens the already-imported program, finds every SCC method (all overloads), decompiles each,
and writes results to `C:\scc-re\out\` (one file per function + a combined `ALL.txt`).

```python
import os
from ghidra.util.task import TaskMonitor
from ghidra.app.decompiler import DecompilerCompositeInterface, DecompileOptions
from ghidra.program.model.symbol import Symbol

OUT = os.path.join(getScriptDir(), "out")
os.makedirs(OUT, exist_ok=True)

TARGETS = [
    "SetRegister", "GetRegister", "SensorOn", "SensorOff",
    "SensorPowerOn", "SensorPowerOff", "MclkOutput", "SetGpio",
    "InitializeControlLogic", "ResetControlLogic",
]

funcMgr = currentProgram.getFunctionManager()
symTab  = currentProgram.getSymbolTable()
monitor = TaskMonitor.DUMMY

dcmgr = DecompilerCompositeInterface(currentProgram, monitor)
opts  = DecompileOptions(None)
dcmgr.setOptions(opts)

written = []

for base in TARGETS:
    syms = symTab.getSymbols(base)
    while syms.hasNext():
        sym = syms.next()
        if not sym.getName() == base:
            continue
        f = funcMgr.getFunctionAt(sym.getAddress())
        if f is None:
            continue
        dgr = dcmgr.decompileComplete(f, monitor)
        if not dgr.decompileSuccess():
            note = "[DECOMPILE FAILED] " + base + ": " + dgr.getErrorMessage() + "\n"
            open(os.path.join(OUT, base + ".log"), "w").write(note)
            continue
        text = dgr.getDecompiledFunction().decompileCode(0)
        # name the file by the class-qualified symbol so overloads don't clobber each other
        qual = f.getLinkerName() if f.getLinkerName() else base
        fn = os.path.join(OUT, qual + ".java")
        open(fn, "w").write(text)
        written.append((base, f.getAddress(), fn))
        print("WROTE " + fn + "   (" + str(f.getAddress()) + ")")

# combined dump for easy grepping
with open(os.path.join(OUT, "ALL.txt"), "w") as allf:
    for base, addr, fn in written:
        allf.write("\n\n=================== " + base + "  @" + str(addr) + " ===================\n\n")
        allf.write(open(fn).read())
print("DONE. " + str(len(written)) + " functions written to " + OUT)
```

> Notes for the agent running this:
> - There are **multiple overloads** of `SetRegister`/`SensorOn`/etc. (one per class). The script
>   keeps all of them, keyed by `getLinkerName()` (e.g. `SSTps68470::SetRegister`). Inspect
>   **all** of them; the camera we care about is the **TPS68470 / `SSTps68470`** path, but confirm
>   which class backs the OV2680 power-on before trusting a value.
> - If `DecompositeInterface` returns `None` (compiler not initialized), the first decompile call
>   triggers init on subsequent calls; retry once. If it still fails, run the Ghidra **GUI**,
>   open the program, and use *Edit → Decompiler* on each function — same data, more verbose.

### 4.3 If decompilation fails entirely

- Check `analyzeHeadless.log` for the Java/Ghidra error.
- Fallback: run Ghidra GUI, open the `.sys`, decompile the methods by right-click → *Decompile*.
- Second fallback: WinDbg (see §6).

---

## 5. Post-process the Ghidra output into the deliverables

From the decompiled `.java`/`ALL.txt`, extract (this is §1):

1. **Port numbers.** In the shared write routine (called by the accessors) find the `out`/`outsb`/
   `outsl` instruction and the value loaded into `%dx`/`%cx` (the port) and any select/index port.
   Report the select-data handshake if present.
2. **Camera-on values.** In `SensorOn`/`SensorPowerOn`/`SSTps68470::SetRegister`, read the immediate
   values written to each register index (`Power0`, `Power1`, `PowerEn`, `Mclk`) and list them in
   call order.
3. **Mclk.** In `MclkOutput`/the Mclk `SetRegister`, identify the enable bit and the rate-select
   field; state the resulting frequency.
4. **Pin map.** In `SetGpio`/`InitializeControlLogic`, map SCC logical lines → `C0P#`/`C0G#` and the
   controlling register/bit. Cross-check against the DSDT `PINR(C0P#, C0G#)` (already in
   `day15`/`day16`).
5. **Adoption.** In `InitializeControlLogic`/`ResetControlLogic`/any CRD `_DSM` handler, write out
   the `CL00`→`C0TP`→`C0GP` order and the exact condition (`CL00 && (C0TP==One)`, then
   `C0GP=N`).

**Save the finished results to `agent_work/scc_data/scc_extracted.md`** (on the pi box, copy it
back after the Windows run), structured as:

```
## SCC register access
- port(s):            e.g. 0x?? (outsb) / select port 0x??
- byte vs dword:      e.g. outsb / outsl
## Camera-on write sequence (in order)
- Power0  (reg idx 0x??): 0x........
- Power1  (reg idx 0x??): 0x........
- PowerEn (reg idx 0x??): 0x........
- Mclk   (reg idx 0x??): 0x........
## Mclk
- enable bit:   ...
- rate field:   ...  -> frequency ...
## C0P#/C0G# pin map
- C0P# (power enable): logical line .. -> reg 0x?? bit ..
- C0G# (general GPIO): logical line .. -> reg 0x?? bit ..
## CRD adoption sequence
- CL00 = ... ; then C0TP = ... (gate: CL00 && (C0TP==One)) ; then C0GP = ...
```

Attach the raw Ghidra decompilations (`out/*.java`, `out/ALL.txt`) alongside it for reference.

---

## 6. WinDbg fallback (if Ghidra won't decompile)

Kernel-mode, so it works on a kernel driver without attaching to a process.

1. Enable kernel debug to a network port on the target:
   `bcdedit /debug on`
   `bcdedit /debugtype net /debugport N /debugportaddress <IP> /debugbaudrate 115200`
   (host: `kd -k net -port <…> -b 115200`), or use a VM with a serial/network debug channel.
2. `ln *SkcController!*SetRegister*` and `*SensorOn*` to locate the routines.
3. `bp <addr>`; when it fires during camera power-on, inspect `%dx` (port), `%edx`
   (register index), and the value actually port-written. Trace `SensorOn` once to capture the
   full ordered sequence.
4. Same deliverables (§1/§5).

---

## 7. Gotchas

- **Multiple overloads** of every method (per class) — decompile all, don't assume one.
- Prefer the **`SSTps68470` / TPS68470** path for our OV2680 (our PMIC is a TPS68470 =
  `PMIC-CRDG`). The `up6641`/other variants are for different sensors.
- objdump on the pi box mis-read data as code and lacks function boundaries — **trust Ghidra/WinDbg
  for the values**, not the day17 objdump numbers.
- This is **kernel-mode** analysis throughout — do *not* try to attach to a user-mode process.
- Read-only until a working Linux path exists: don't modify anything on the Windows system beyond
  copying the driver and running Ghidra/WinDbg.
