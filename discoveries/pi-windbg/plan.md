# plan.md — windbg (kernel) dynamic-analysis extension for pi-ghidra

Elaboration of `idea.md`. Working plan for a pi extension that keeps a live,
steerable kernel-debug session and correlates it back to pi-ghidra's
decompilation. **Backend = windbg; v1 = kernel-only.** See `idea.md` for the
why and the locked decisions.

**Status: finalized — fully frozen. Both Section-9 blockers are resolved**
(#1 pi-on-box confirmed; #2 ghidra export confirmed live). Structure, phases,
data model, and typed API are locked. No open architecture questions remain; this
is ready to execute.

**Changelog**
- v1 (final): frozen; Section 9 blockers carry recommended defaults; checklist
  in Section 10 aligned to the idea.md decision log.
- v1.1: decision #2 verified live — pi-ghidra exports `functions` (absolute,
  image-base-inclusive `entry`) against the real Skccontroller.sys. Seeding is
  cheap; correlation reduced to a base compare. Updated §2/§3/§4/§5/§6(P4)/§9.
- v1.2: target environment corrected to the **local target box** (single-box
  boot debug, no separate VM), matching decision #1. All VM references in §1,
  §6(P0), and the risk table (§8) updated.

---

## 0. Restated goal (precise)
Give pi the ability to **drive a kernel-mode debugger and read what actually
executed**, then **map that back onto ghidra's static decompilation** so static
code-flow is annotated with the paths that ran, the branch taken, and the live
values. v1 proves this end-to-end on `skccontroller.sys`.

### Non-goals (v1 — hold the line)
- No userspace debugger. Other debuggers already cover it; not our edge.
- No generic multi-backend. windbg is the engine; the abstraction is a
  convenience, not a plugin host.
- No GUI. This is a pi-facing, scriptable module.
- No reverse-engineering-of-the-correlation for unsigned/unbuilt binaries we
  don't also have a symbol source for.

---

## 1. Target environment
- **Kernel debug:** single-box boot debugging on the **local target machine**
  (`bcdedit /debug`, then reboot into debug mode). Non-negotiable; userspace-only
  windbg is out of scope. The local box boots into its own debug session — no
  separate VM.
- **Automation layer:** **pykd** (ships with windbg/WDK). It scripts the
  debugger in kernel mode — breakpoints, register/memory read+patch, session
  control, symbol lookup — far more than a `windbg -c` pipe.
- **Target box:** Windows (runs the driver). Everything lives on the target.
- **Signing:** local box in test-signing mode so an unsigned `skccontroller.sys`
  build loads for analysis.

### pykd facts we build on
- `pykd.getModBaseEx("<modname>")` — runtime load base of a loaded module.
- `pykd.addBPatch(addr, cb)` / `pykd.bp(addr)` — breakpoints + hit callbacks.
- `pykd.reg("rcx")`, `pykd.loadQWord(addr)`, `pykd.storeQWord(addr, v)` —
  read/patch registers and memory (this is the pin-simulation primitive).
- `pykd.stackWalk()` / `pykd.getStackWalk()` — live call stack.
- `pykd.getSymbol(addr)` / `ln addr` — symbol at an address.
- `pykd.dbgCommand("...")` — escape hatch for anything pykd doesn't wrap.

> Spike these first (Phase P1). Confirm the channel before designing the product.

---

## 2. Architecture

```
        pi (on target box)
          │  calls typed ops / reads JSON state
          ▼
   ┌───────────────────────────┐
   │  DebugSession (state)      │  ← persisted to disk each mutation
   │  - modules (base map)      │
   │  - breakpoints / hypotheses│
   │  - last stack/regs/mem snap│
   └───────────┬───────────────┘
               │  pykd API
               ▼
   ┌───────────────────────────┐
   │  pykd adapter              │  error handling, off-thread callbacks,
   │  - set_bp / run / get_*    │  dbgCommand escape hatch, symbol lookup
   └───────────┬───────────────┘
               │  attaches to live kernel-debug windbg
               ▼
         windbg (kernel debug mode)  ◄──  target driver skccontroller.sys
```

Two pillars (from `idea.md`):

### Pillar A — Steerable stateful session
The debugger is a **first-class object pi owns**, not a command runner.
- Typed ops (Section 4) on a live session.
- Persistent **hypothesis/breakpoint list** that survives across pi turns —
  this is the product, windbg is the engine.
- On each breakpoint hit, capture a **snapshot**: registers, full stack, and
  a targeted memory read (e.g. the pin register) keyed by the hitting bp.

### Pillar B — Correlation engine
Map **live runtime addresses → static ghidra addresses**.
Because pi-ghidra `functions[].entry` is absolute/image-base-inclusive, this is a
*base offset* problem, not an offset-subtraction problem:
```
gidra_entry == runtime_addr      when runtime_base == image_base
runtime_addr = runtime_base + (gidra_entry - image_base)   otherwise
```
- Per module store: `image_base` (from pi-ghidra `info.imageBase`, e.g.
  `0x140000000`), `runtime_base` (windbg `lm`/`!pe`), and a symbol map keyed by
  absolute address.
- Import the driver's **.pdb into ghidra** so ghidra symbol names line up with
  windbg symbol names **by absolute address** — cleanest correlation, preferred
  over offset-only matching.
- The engine emits, for any live frame: the runtime addr, the windbg symbol, and
  the mapped ghidra function + absolute address + decompiled context.
- **Drift signal:** if `runtime_base != image_base`, every `entry` maps by the
  delta. A *shifting* base across reboots is what `drift_warning` catches
  (Section 8).

> Correlation is make-or-break (Section 8). It gets a dedicated phase.

---

## 3. Data model (persisted JSON, one file per session)

```jsonc
{
  "session": { "id": "...", "created": "...", "windbg_pid": 420 },
  "modules": [
    {
      "name": "skccontroller",
      "image_base": "0x140000000",        // from pi-ghidra info.imageBase
      "runtime_base": "0xffffa03b12300000", // from windbg lm / !pe
      "static_image_size": "0x14000",
      "symbols": { "0x140001480": "CameraPowerUp", "0x140001000": "PinInitialize" },
      "correlated": true,            // set when base map is validated
      "drift_warning": false         // runtime_base drifted vs last session
    }
  ],
  "hypotheses": [
    { "id": "H1", "text": "pin reader routine", "status": "open" }
  ],
  "breakpoints": [
    {
      "id": "bp1", "module": "skccontroller",
      "static_addr": "0x140001480",      // pi-ghidra entry (absolute)
      "runtime_addr": "0xffffa03b12301480", // windbg (absolute)
      "hypothesis": "H1",
      "hit_count": 3,
      "last_hit": {
        "regs": { "rcx": "0x2", "rax": "0x140001480" },
        "stack": [ { "addr": "0xffffa03b12301480", "symbol": "skccontroller!CameraPowerUp" } ],
        "memory": { "pin_register": { "addr": "0x...", "value": "0x5" } }
      }
    }
  ]
}
```

Design rules:
- **Write the file on every mutation** — pi can re-read state each turn.
- All addresses stored as **strings** (JSON has no 64-bit int) and **absolute**
  (both windbg runtime and pi-ghidra static live in the same absolute space,
  so bp seeding is a 1:1 map when `runtime_base == image_base`).
- `correlated`/`drift_warning` force the engine to *prove* the map on load: if
  `runtime_base != image_base`, every static addr maps via the delta and the
  drift is flagged.

---

## 4. Typed API surface (what pi calls)

| Op | Purpose | Returns |
|----|---------|---------|
| `connect()` | attach to the live kernel-debug windbg session | module list + validated base map |
| `set_bp(addr | "module!sym", hypothesis?)` | add a breakpoint, optionally tagged with a hypothesis | `{bp_id, runtime_addr}` |
| `run()` | resume until next bp hit (blocks) | `{bp_id, regs, stack[], memory{}}` hit snapshot |
| `get_regs()` | read current registers | `{name: value}` |
| `get_stack()` | walk live stack | frames `[addr, symbol, offset]` |
| `read_memory(addr, size, type?)` | read bytes/quadword/string, typed | decoded value |
| `patch_memory(addr, value)` | overwrite mem/regs (pin simulation) | previous value (for restore) |
| `list_bps()` / `remove_bp(bp_id)` | manage the hypothesis list | bp list |
| `correlate(static_addr)` | map a ghidra absolute addr → runtime | `{runtime_addr, symbol, module}` |
| `propose_bps(functions[])` | seed bps from pi-ghidra `functions` (absolute `entry`) | created bp_ids |

`propose_bps()` takes pi-ghidra function `entry` values (absolute) and, when
`runtime_base == image_base`, uses them verbatim as bp addresses; otherwise it
subtracts the expected base. No offset tables needed.

`run()` is the only blocking op; it returns a fully captured snapshot so pi can
correlate without re-querying the live debugger.

---

## 5. The ghidra → windbg seeding seam
The loop must *start* from ghidra. **CONFIRMED — no list-production work item.**
pi-ghidra's `functions` action already exports a machine-readable list
`[{name, entry, signature, callingConvention, returnType, params, locals}]`;
`info` adds `imageBase` + `functionCount`. Verified live against the real
`Skccontroller.sys` (417 functions, 3528 symbols). `propose_bps()` consumes this
list directly (paged: `limit` cap 2000, `offset`).

**Address model (important).** pi-ghidra `entry` is an **absolute,
image-base-inclusive** address (Ghidra loads the PE at its compiled image base by
default). Correlation is therefore a *direct compare*, not offset subtraction:

```
gidra_entry == runtime_addr   when the driver loads at its compiled image base
runtime_addr = runtime_base + (gidra_entry - image_base)
```

So on load windbg records `runtime_base`; if `runtime_base == image_base`
(`info.imageBase`, e.g. `0x140000000`) the two address spaces line up 1:1 and
`entry` *is* the bp address. **Any divergence is the drift signal** — that's
Section 8's `drift_warning`.

Output flows the other way: correlation annotations (which functions actually
ran, with values) can be written back as ghidra comments/annotations or emitted
as JSON that pi reads.

---

## 6. Development phases / milestones

**P0 — Environment.** Local target box in test-signing mode, `bcdedit /debug`
kernel debug up (reboot into debug mode), pykd importable. Gate for everything
after.

**P1 — pykd spike.** On a trivial signed test kernel object with a *known* path:
`set_bp` → `patch_memory(reg)` → `run()` → `get_stack()` → assert the expected
function is on the stack. This validates the automation channel and the
pin-simulation primitive end-to-end. **No product code yet.**

**P2 — Session manager core.** DebugSession state model (Section 3), JSON
persistence, typed ops (Section 4) on the pykd adapter with off-thread
callback handling and error translation.

**P3 — Correlation engine.** Base-address math, symbol alignment (prefer .pdb),
offset mapping, drift detection on load. Unit-test the math with a synthetic
module — no debugger needed.

**P4 — Ghidra seam.** Ingest pi-ghidra's already-exported `functions` list,
`propose_bps()` (absolute-addr seeding), write-back annotations. **No list-
production work item** — the export exists (Section 5).

**P5 — v1 scenario.** Full `skccontroller.sys` walkthrough: pin-reader bp →
simulate pin combos → map the pin→camera decision tree, correlated onto ghidra.

**P6 — Polish.** Error handling, docs, and — only if pi runs off-box — the
relay/transport; pi-on-box is locked (decision #1, Section 9).

---

## 7. Testing & verification
- **Correlation math** (P3): unit tests with synthetic modules, assert the
  base-compare round-trips — `entry == runtime_addr` when `runtime_base ==
  image_base`, and the delta mapping otherwise.
- **Known-path test** (P1/P5): force a specific pin combo, assert the captured
  stack contains the *expected* function — a ground-truth check we control.
- **Drift test:** change the loaded base (e.g. reload), assert the engine flags
  `drift_warning` and rebuilds the map.
- Prefer a **synthetic signed test driver** with a predictable pin→action map
  before investing in `skccontroller.sys` (whose real wiring we don't know).

---

## 8. Risks & mitigations

| Risk | Severity | Mitigation |
|------|----------|-----------|
| Correlation mismatch — windbg addr ≠ ghidra addr | high | Engine *proves* the map on load; emit a mismatch alert with both bases; prefer .pdb symbol alignment |
| Base address drift across reboots | high | Record base per session; detect drift; rebuild map; pin driver load order / confirm fixed base |
| pykd callback reentrancy / hangs | med | Run debugger commands off the hit-callback thread; wrap in try/finally; timeout `run()` |
| Pin path not patchable (IO port vs memory) | med | Detect read path; if IO port, bp before the read and read IO space, or drive manually |
| Kernel-debug disrupts the box | med | Single-box reboot is expected; use a dedicated target box, not your daily-driver workstation |
| Driver signing blocks load | med | Test-signing mode / signed test driver |
| pi-off-box relay adds failure surface | med | Defer to P6; prefer pi-on-box (Section 9) |

---

## 9. Resolved decisions (both closed)
> Both items from the original "open decisions" are now settled. See `idea.md`
> §Decision log for the canonical record.
1. **pi-on-box vs Linux bridge.**
   - **Recommended: pi on the Windows target** → local pykd module, no bridge.
   - pi on Linux → relay + transport (deferred to P6). Bigger scope.
   - *This fork decides the whole deployment shape.* Resolve before P2.
2. **Does pi-ghidra export a function/addr list?**
   - **RESOLVED — yes (tested live).** `functions` action emits `{name, entry,
     signature, …}`; `info` adds `imageBase`. Seeding is cheap; P4 is small.
   - The `entry` is absolute/image-base-inclusive, so correlation is a direct
     base compare (Section 5/2). Nothing more to determine here.

## 10. Immediate next session checklist
- [ ] **Decision #1 (pi-on-box)** confirmed — local pykd, no bridge.
- [ ] **Decision #2 (ghidra export)** confirmed — `functions` export exists.
- [ ] Confirm fixed driver load base at runtime == compiled image base
      (`0x140000000`) + a patchable pin-read path.
- [ ] Run P1 pykd spike on a trivial test driver.
- [ ] Run P1 pykd spike on a trivial test driver.
- [ ] If spike passes, start P2 (session manager).
