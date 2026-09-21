# chezmoi ↔ Evolution sources: the `LastNotified` drift problem

**Status:** Resolved — user picked the re-add path; implemented as a standalone
script (see below). The inherent contradiction is only worked around, not
solved: the live sources still carry `LastNotified`, so `status`/`diff` can
still flag them after Evolution rewrites the key.
**Related:** `dot_config/private_evolution/private_sources/`

## Context

Evolution source files live in `~/.config/evolution/sources/*.source`
(plus `system-calendar.source`). They contain a runtime-mutated key:

```
LastNotified=2026-09-10T14:50:00Z
```

chezmoi manages the base `.source` files (encrypted `encrypted_*.source.age`
in the repo → decrypted on apply). When Evolution writes `LastNotified`, the
on-disk file diverges from the committed template, so `chezmoi status`/`diff`
flag them as modified, and a later template change risks overwriting the value.

### The current workaround (two run scripts, NOT in `.chezmoiscripts`)

- `run_before_any_evolution_file_applies.sh` — before apply, strip the
  `LastNotified=` line from every source and save it to a `$f.lastnotified`
  sidecar.
- `run_after_z_last_file_was_applied.sh` — after apply, restore the value
  from the sidecar back into the `.source`.

Both were validated in a sandbox against live sources: before-strip makes all
`.source` files clean, after-script restores the exact values. The round-trip
works and is idempotent.

## The real problem (confirmed)

`run_after_*` restoration makes the files "modified" again, and there is **no**
"later after" to hide it behind.

`chezmoi apply` order (https://chezmoi.io/reference/application-order/):

1. Read source state
2. **Read destination state** ← drift is detected here
3. Compute target state
4. Run `run_before_*` scripts
5. Update files (the apply)
6. Run `run_after_*` scripts ← **final step, nothing after it**

Drift is verified at step 2 of the *next* invocation, and `status`/`diff` are
read-only (they run no before/after scripts). So any value the after-script
re-inserts is always visible as a pending change on the next
`status`/`diff`/`apply`. Also: chezmoi assumes the destination isn't mutated
mid-run — the before/after scripts violate that (undefined behavior), though it
harmlessly works because the before-script cleans the file before step 5.

## Options to resolve (inherent contradiction: a changing runtime key can't
both live in a managed file *and* keep `status` clean)

1. **Accept cosmetic drift** (current approach). `apply` runs clean (no
   prompt); only `status`/`diff` mark the files. Normal for app-mutated
   config. Keeps the `LastNotified` value. — *recommended as-is*
2. **Drop the after-script** — let before-strip remove `LastNotified` and
   never restore. Post-apply file matches template → `status` clean until
   Evolution rewrites the key again. Value is lost, but it's just a runtime
   cookie (Evolution re-notifies once). Simplest clean-status option.
3. **Exclude from chezmoi** — add `~/.config/evolution/sources/**` to
   `.chezmoiignore` and manage those files another way. No drift ever; lose
   encrypted-template management of the base files.
4. **`modify_` script** — strip `LastNotified` during target computation.
   Most "chezmoi-native," but only strips (same value-loss as #2). Verify
   modify scripts also participate in `status`/`diff` target computation
   before relying on it.

## Decision

Resolved 2026-09-13: user chose the re-add path (effectively option #2). A
standalone script, `update-evolution-sources.sh`,
re-strips `LastNotified`, re-adds the changed sources to chezmoi (removing
`LastNotified` from the committed templates), then restores it. This differs
from #2 only by *also* restoring the live value (so the running app keeps its
cookie between runs) and by only touching sources that genuinely changed.

### Implemented: `update-evolution-sources.sh`

- **Source:** `dot_local/bin/executable_update-evolution-sources.sh` →
  `~/.local/bin/update-evolution-sources.sh` (run manually, not a
  `.chezmoiscripts` hook).
- Reuses the two existing hooks for strip/restore (single source of truth).
- **Re-encryption guard (important):** `age` ciphertext is non-deterministic,
  and `chezmoi re-add` re-encrypts a *dirty* destination's source on every
  call, so blindly re-adding leaves git noise even with no content change.
  The script therefore `chezmoi cat`s the decrypted committed template and
  `diff`s it (both sides with `LastNotified` stripped) against the live file,
  re-adding **only on a genuine difference**. Verified idempotent: first run
  strips `LastNotified` from the 2 templates that carry it, subsequent runs
  re-add nothing.
- `status`/`diff` cleanliness is NOT guaranteed by this script — once
  Evolution writes a fresh `LastNotified`, the live file is dirty again.

## Notes / gotchas

## Verification commands

```bash
# current apply status (should be clean apart from these files)
cz status | grep -i evolution

# confirm no post-apply hook exists
chezmoi --help | grep -iE 'post|after'   # only run_after_* exists, no later hook
```

## Notes / gotchas

- Neither run script is wired into `.chezmoiscripts` and nothing else invokes
  them — the trigger that runs them before/after apply is unclear; verify it
  actually fires. (`update-evolution-sources.sh` invokes them directly instead.)
- The before-script uses `for f in $(find …)` — word-splitting breaks on
  filenames with spaces. Current Evolution source names (hex/hyphen) are safe,
  but `find -print0 | while IFS= read -r -d ''` would be more robust.
- `*.lastnotified` sidecars are unmanaged runtime artifacts in the sources dir.
