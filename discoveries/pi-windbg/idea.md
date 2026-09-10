# pi-extension: windbg (kernel) as a dynamic-analysis enhancer for pi-ghidra

**Status:** hardened idea — pick up tomorrow on the target box.
**Origin:** forge session in `pi-windbg`. Core question answered: *yes, worth it* — but only as a stateful, steerable dynamic layer that complements ghidra's static view. Not a `windbg -c` wrapper.

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

## Open decisions (need your call)
1. **Where does pi run?** On the Windows target box → local pykd module, no bridge (recommended). OR bridged from Linux → you ship a relay service + transport (bigger scope, more failure surface). *Biggest architectural fork.*
2. **ghidra→windbg seeding seam.** The loop must start from ghidra. Does pi-ghidra already export a machine-readable function list w/ addresses to seed breakpoint candidates? If yes → design around it. If no → exporting that list is budgeted v1 work.

## Weak points / risks (watch these)
- **Correlation is make-or-break.** If windbg addresses don't line up with ghidra's, you get great stacks for unlocatable code. Nail the base-address math early.
- **Local base-address stability** across reboots (`bcdedit /debug` sessions). Confirm the driver loads at a fixed address; a shifting base reintroduces the correlation problem.
- **Kernel-debug env required** — single-box boot-debug (`bcdedit /debug`, reboot) or VM with shared serial. Non-negotiable for kernel.
- **Pin simulation** needs a *patchable* read path — verify the driver reads pins through a register/memory location you can overwrite, not opaque hardware IO.

## Next session (tomorrow)
1. Decide pi-on-box vs Linux bridge (#1).
2. Check whether pi-ghidra exports a function/addr list (#2).
3. Confirm fixed driver load base + a patchable pin-read path.
4. Spike: pykd set_bp → patch reg → run → get_stack, on a trivial kernel object, before touching `skccontroller.sys`.
