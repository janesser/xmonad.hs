# plan.md — windbg (kernel) dynamic-analysis extension for pi-ghidra

Elaboration of `idea.md`. Working plan for a pi extension that keeps a live,
steerable kernel-debug session and correlates it back to pi-ghidra's
decompilation. **Backend = windbg; v1 = kernel-only.** See `idea.md` for the
why and the locked decisions.

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
- **Kernel debug:** single-box boot debugging (`bcdedit /debug`, reboot) on a
  dedicated test VM, or a Windows VM with a host↔guest shared-serial debug link.
  Non-negotiable; userspace-only windbg is out of scope.
- **Automation layer:** **pykd** (ships with windbg/WDK). It scripts the
  debugger in kernel mode — breakpoints, register/memory read+patch, session
  control, symbol lookup — far more than a `windbg -c` pipe.
- **Target box:** Windows (runs the driver). Everything lives on the target.
- **Signing:** test VM in test-signing mode so an unsigned `skccontroller.sys`
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
Map **live runtime addresses → static ghidra offsets**:
```
runtime_addr = module.runtime_base + ghidra_static_offset
```
- Per module store: `runtime_base` (windbg), `static_image_base` (PE header /
  .pdb), and a symbol map.
- Import the driver's **.pdb into ghidra** so ghidra symbol names line up with
  windbg symbol names by offset — the cleanest correlation, preferred over
  offset-only matching.
- The engine emits, for any live frame: the runtime addr, the windbg symbol,
  and the mapped ghidra function + offset + decompiled context.

> Correlation is make-or-break (Section 8). It gets a dedicated phase.

---

## 3. Data model (persisted JSON, one file per session)

```jsonc
{
  "session": { "id": "...", "created": "...", "windbg_pid": 420 },
  "modules": [
    {
      "name": "skccontroller",
      "runtime_base": "0xffffa03b12300000",
      "static_image_base": "0x180001000",
      "static_image_size": "0x14000",
      "symbols": { "0x1000": "PinInitialize", "0x1480": "CameraPowerUp" },
      "correlated": true,            // set when base map is validated
      "drift_warning": false         // base changed vs last session
    }
  ],
  "hypotheses": [
    { "id": "H1", "text": "pin reader routine", "status": "open" }
  ],
  "breakpoints": [
    {
      "id": "bp1", "module": "skccontroller", "static_offset": "0x1480",
      "runtime_addr": "0xffffa03b12301480", "hypothesis": "H1",
      "hit_count": 3,
      "last_hit": {
        "regs": { "rcx": "0x2", "rax": "0x18001480" },
        "stack": [ { "addr": "...", "symbol": "skccontroller!CameraPowerUp", "offset": "0x1480" } ],
        "memory": { "pin_register": { "addr": "0x...", "value": "0x5" } }
      }
    }
  ]
}
```

Design rules:
- **Write the file on every mutation** — pi can re-read state each turn.
- All addresses stored as **strings** (JSON has no 64-bit int).
- `correlated`/`drift_warning` force the engine to *prove* the map on load.

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
| `correlate(static_offset)` | map a static offset → runtime | `{runtime_addr, symbol, module}` |
| `propose_bps(functions[])` | seed bps from a ghidra function list | created bp_ids |

`run()` is the only blocking op; it returns a fully captured snapshot so pi can
correlate without re-querying the live debugger.

---

## 5. The ghidra → windbg seeding seam
The loop must *start* from ghidra. Two options for the input:
1. **pi-ghidra exports** a machine-readable function list:
   `[{module, name, static_offset, size}]` — ideal, design around it.
2. **No export available** → budget v1 work to produce this list
   (from the .pdb, or from ghidra's function table). Either way, this list is
   the candidate pool that `propose_bps()` turns into breakpoints via the
   correlation map (`runtime_addr = base + offset`).

Output flows the other way: correlation annotations (which functions actually
ran, with values) can be written back as ghidra comments/annotations or emitted
as JSON that pi reads.

---

## 6. Development phases / milestones

**P0 — Environment.** Test VM in test-signing mode, `bcdedit /debug` kernel
debug up, pykd importable. Gate for everything after.

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

**P4 — Ghidra seam.** Ingest the function list, `propose_bps()`, write-back
annotations.

**P5 — v1 scenario.** Full `skccontroller.sys` walkthrough: pin-reader bp →
simulate pin combos → map the pin→camera decision tree, correlated onto ghidra.

**P6 — Polish.** Error handling, docs, and — only if pi runs off-box — the
relay/transport (Section 8, open decision #1).

---

## 7. Testing & verification
- **Correlation math** (P3): unit tests, synthetic modules, assert
  `runtime = base + offset` round-trips.
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
| Kernel-debug disrupts the box | med | Dedicated test VM, never the working box |
| Driver signing blocks load | med | Test-signing mode / signed test driver |
| pi-off-box relay adds failure surface | med | Defer to P6; prefer pi-on-box (Section 9) |

---

## 9. Open decisions (blockers — resolve first)
1. **pi-on-box vs Linux bridge.** pi on the Windows target → local pykd module,
   no bridge (recommended). pi on Linux → ship a relay service + transport
   (P6); bigger scope. *This fork decides the whole deployment shape.*
2. **Does pi-ghidra export a function/addr list?** If yes, P4 is cheap; if no,
   producing that list is v1 work.

## 10. Immediate next session checklist
- [ ] Resolve decision #1 (pi location) and #2 (ghidra export).
- [ ] Confirm fixed driver load base + a patchable pin-read path.
- [ ] Run P1 pykd spike on a trivial test driver.
- [ ] If spike passes, start P2 (session manager).
