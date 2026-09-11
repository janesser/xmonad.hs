# Epic 1: windbg (kernel) dynamic-analysis extension for pi-ghidra

A pi extension that keeps a **live, steerable kernel-mode debug session** and
**correlates it back to pi-ghidra's static decompilation**, so static code-flow
is annotated with the paths that ran, the branch taken, and the live values.
v1 proves this end-to-end on `skccontroller.sys`.

Locked decisions (see `idea.md` for the full rationale):

- **Backend = windbg**; v1 is **kernel-only**.
- **pi runs on the Windows target** — local pykd module, no Linux bridge.
- **Backbone = pykd** (breakpoints, register/memory read+patch, session
  control, symbol lookup).
- **Correlation = absolute-address base compare.** pi-ghidra's `functions`
  action exports `{name, entry, …}` where `entry` is absolute and
  image-base-inclusive, so `windbg_runtime == ghidra_entry` when the driver
  loads at its compiled image base; any divergence is the drift signal.
- v1 has no userspace debugger, no generic multi-backend, and no GUI.

Non-goals: userspace debugging, generic multi-backend, GUI, and
reverse-engineering correlation for unsigned/unbuilt binaries with no symbol
source.

---

### Story 1.0: P0 — debug environment

Enable single-box boot kernel debugging **on the local target machine**
(`bcdedit /debug`, then reboot into debug mode) with test-signing on so an
unsigned `skccontroller.sys` build loads. This is the gate for everything after
it; userspace-only windbg is out of scope. No separate VM — the local box boots
into its own debug session.

### Story 1.1: P1 — pykd spike

On a trivial signed test kernel object with a **known path**, validate the
automation channel and the pin-simulation primitive end-to-end:
`set_bp` → `patch_memory(reg)` → `run()` → `get_stack()`, assert the expected
function is on the stack. No product code yet.

### Story 1.2: P2 — session manager core

`DebugSession` state model with JSON persistence (write on every mutation) and
the typed API (`connect`, `set_bp`, `run`, `get_regs`, `get_stack`,
`read_memory`, `patch_memory`, `list_bps`, `remove_bp`, `correlate`,
`propose_bps`) over the pykd adapter, with off-thread callback handling and
error translation.

### Story 1.3: P3 — correlation engine

Absolute-address base-compare math against pi-ghidra `image_base`, symbol
alignment (prefer `.pdb`), offset mapping, and drift detection on load.
Unit-tested with a synthetic module — no debugger needed.

### Story 1.4: P4 — ghidra seam

Ingest pi-ghidra's already-exported `functions` list (absolute `entry`),
`propose_bps()`, and write-back correlation annotations as ghidra comments or
JSON.

### Story 1.5: P5 — v1 scenario

End-to-end `skccontroller.sys` walkthrough: pin-reader bp → simulate pin
combos → map the pin→camera decision tree, correlated onto ghidra.

### Story 1.6: P6 — polish

Error handling and documentation. No relay/transport work (pi-on-box is locked).
