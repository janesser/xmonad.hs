# pi-extension: windbg (kernel) as a dynamic-analysis enhancer for pi-ghidra

**Status:** **finalized — frozen.** Locked, reviewed, and internally consistent.
Pick up tomorrow on the target box.
**Origin:** forge session in `pi-windbg`. Core question answered: *yes, worth it* — but only as a stateful, steerable dynamic layer that complements ghidra's static view. Not a `windbg -c` wrapper.

> **Finalization note:** both prior blockers are now **resolved** — decision #1
> (pi on the Windows target) and decision #2 (ghidra `functions` export exists,
> verified live). Both docs are fully frozen. No open architecture questions
> remain; tomorrow is execution (base/patch-path check, then P1 spike).

**Changelog**
- v1 (final): status frozen; open-decision section converted to a decision log
  with recommended defaults; weak-points hardened.
- v1.1: both blockers resolved. Decision #1 confirmed — pi on the Windows
  target (local pykd, no bridge). Decision #2 verified live — pi-ghidra
  exports `functions` (absolute, image-base-inclusive `entry`); correlation is
  a base compare. Next session narrowed to base/patch-path check + P1 spike.
- v1.2: target environment corrected to the **local target box** (single-box
  boot debug, no separate VM). Story 1.0 in the sprint epics aligned.
- v1.1: both blockers resolved. Decision #1 confirmed — pi on the Windows
  target (local pykd, no bridge). Decision #2 verified live — pi-ghidra
  exports `functions` (absolute, image-base-inclusive `entry`); correlation is
  a base compare. Next session narrowed to base/patch-path check + P1 spike.

---

## The one-line pitch
A pi extension that keeps a **live, steerable kernel-debug session** and **correlates it back to pi-ghidra's decompilation** — so static code-flow gets annotated with what actually ran, in which branch, with what values.

## Why an extension (not direct `windbg` CLI)
`windbg -c "cmd" target` is one-shot: no memory, no state, context lost on exit.
We need what the CLI can't give:
- **Stateful breakpoint loops** — bp a function, read live state, bp the next, across many turns.
- **Interactive steering** — patch register/memory to *simulate the physically-wired pins* and force each branch to execute, mapping the whole decision tree without real hardware.
- **Correlation** — map windbg's live addresses/symbols onto ghidra's static graph.

## Locked decisions
- **Backend: windbg first.** Generic/multi-backend deferred.
- **Mode: kernel-only in v1.** Userspace breakpoint debugging is already covered by other debuggers; windbg's unique value + pi-ghidra's gap is kernel. Don't spend v1 where you're not unique.
- **Backbone: pykd** (scripts windbg in kernel mode — breakpoints, read/patch memory & registers, session control). Revisit direct `-c` pipe only if pykd falls short.
- **Product = two pillars:**
  1. *Steerable stateful session* — typed ops: `set_bp`, `run`, `get_stack`, `get_regs`, `read_memory`, `patch_memory`, and a persistent breakpoint/hypothesis list. The session state + hypothesis list IS the product; windbg is the engine.
  2. *Correlation engine* — align live debugger addresses/symbols to ghidra's static image. Resolve via windbg load base (`lm` / `!pe` / `LDR_DATA_TABLE_ENTRY`) vs. the PE's compiled image base (`.pdb` or `dumpbin /headers`). Do it once, parameterized.
- **Deployment target: Windows box** (runs the driver). Consistent with everything living on the target.

## Killer v1 scenario (the proof-of-value)
`skccontroller.sys`: driver powers cameras from physically-wired pins.
Ghidra reveals the pin-reader + all camera-power branches (the whole tree). In a dev box the pins aren't wired, so: bp the pin-reader → patch the pin-read register/memory to each fake pin state → resume → capture which camera path fires. Walk every combo → exhaustively map the pin→camera decision tree from live stacks, correlated onto ghidra's static graph.

## Decision log (finalized)
1. **Backend = windbg.** Locked. Multi-backend deferred.
2. **Mode = kernel-only v1.** Locked. Userspace is someone else's edge.
3. **Backbone = pykd.** Locked. Revisit direct `-c` pipe only if pykd falls short.
4. **Deployment box. CONFIRMED: pi on the Windows target.** Local pykd module,
   no bridge — everything already lives on the target; no transport failure
   surface. This fork sets the whole deployment shape; do not re-introduce a
   Linux relay without a compelling reachability reason.
5. **ghidra seeding seam. CONFIRMED — export exists (tested live).**
   pi-ghidra's `functions` action already emits a machine-readable list,
   per function: `{name, entry, signature, callingConvention, returnType, params, locals}`.
   `info` adds `imageBase` and `functionCount`. Confirmed against the real
   `SkcController.sys` (417 functions, 3528 symbols). `propose_bps()` is cheap —
   **design around the existing export; no v1 list-production work item.**
   - **Key property:** `entry` is an **absolute, image-base-inclusive** address
     (Ghidra loads the PE at its compiled base). So correlation is a direct
     compare (`windbg_runtime == ghidra_entry` when loaded at default base), and
     any divergence *is* the drift signal — simpler than the plan's
     `runtime = base + offset` model. See plan §2/§3.

## Weak points / risks (watch these)
- **Correlation is make-or-break.** If windbg addresses don't line up with ghidra's, you get great stacks for unlocatable code. Nail the base-address math early.
- **Local base-address stability** across reboots (`bcdedit /debug` sessions). Confirm the driver loads at a fixed address; a shifting base reintroduces the correlation problem.
- **Kernel-debug env required** — single-box boot-debug (`bcdedit /debug`, reboot) or VM with shared serial. Non-negotiable for kernel.
- **Pin simulation** needs a *patchable* read path — verify the driver reads pins through a register/memory location you can overwrite, not opaque hardware IO.

## Next session (tomorrow)
1. **Confirm the driver's load base at runtime** (windbg `lm` / `!pe`) equals
   the compiled image base Ghidra used (`0x140000000`); lock the correlation to
   this. Confirm a patchable pin-read path exists.
2. **Run the P1 pykd spike** on a trivial signed test driver with a known path:
   `set_bp` → patch reg/memory → `run` → `get_stack`, assert the expected
   function is on the stack, before touching `skccontroller.sys`.
