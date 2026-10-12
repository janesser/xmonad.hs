# mise `pypi:` backend — installing PyPI CLIs declaratively

> Forward-looking note. The capability described below is **not available** in
> the mise currently pinned on this box (`v2026.9.3`). It landed in
> **v2026.9.7** (2026-09-13). This records what becomes possible so the
> follow-up (upgrade mise, then switch the `hf` install over) is trivial to
> pick up.

Status: **held — parked on a mise upgrade.** Reference note for a future mise
capability with a concrete next step (upgrade mise ≥ v2026.9.7, then replace
the run_once `hf` installer with a `[tools]` entry). No imminent action: mise
self-update is gated by `minimum_release_age` (24h default).

Related: `.chezmoiscripts/run_once_5_aitools_4hf_install.sh` (the current,
working workaround — `mise exec -- uv tool install hf`, committed `fe0515c`).

---

## TL;DR

From **mise v2026.9.7** onward, mise ships a **`pypi:` backend** so any PyPI
command-line tool can be installed *declaratively* in `mise.toml` — no run
script, no `uvx`, no one-off command:

```toml
[tools]
"pypi:hf" = "latest"
```

`mise install` then installs it, using **`uv tool install hf`** under the hood
whenever `uv` is available (`pypi.uvx`, default `true`; set `uvx = false` to
fall back to `pipx`). This is "uv:tools" in practice — just expressed as a
`pypi:` tool, not a `[uv:tools]` section (mise has no such section).

---

## What becomes possible once mise ≥ v2026.9.7

- **Declarative PyPI tools.** Add any PyPI CLI next to the native tools already
  in `dot_config/mise/config.toml` (`uv`, `helm`, `kubectl`, …). It is
  version-pinned, reproduced across machines, and picked up by `mise install`
  / fresh `mise bootstrap` runs — no run_once script.
- **`hf` in this repo** becomes a one-liner:

  ```toml
  [tools]
  "pypi:hf" = "latest"
  ```

  …replacing `.chezmoiscripts/run_once_5_aitools_4hf_install.sh`. mise installs
  `hf` to `~/.local/bin`, which is already on PATH, so `hf` runs directly
  (the old `hf.fish` `uvx hf` wrapper is obsolete — already removed).

- **Richer options** (v2026.9.8–9.9): dependency-graph locking in `mise.lock`
  (via uv), version-only fallback when a graph can't be built, and `with` /
  `expose` / `dependency_prereleases` for extras and optional deps.

## Why not now

- The box runs **v2026.9.3** (2026-09-08), which predates the pypi backend.
  `mise backends ls` there lists `pipx` / `pkgx` / `packslip` but **no
  `pypi`**, and `["pypi:hf"]` errors with `pypi is not a valid plugin name`.
- The pypi backend was introduced in **v2026.9.7** (PRs #13130/#13170/#13181,
  release note: *"Lockfile revision 2 with npm and Python dependencies (via
  uv)"*), refined in v2026.9.8 (version-only fallback) and v2026.9.9
  (lock-aware `with`/`expose`/`dependency_prereleases`). Latest is
  v2026.10.8.
- mise self-update respects `minimum_release_age` (24h default), so upgrading
  off v2026.9.3 may take a day or two to land unless forced.

## Next step (when picked up)

1. Upgrade mise to ≥ v2026.9.7 (force if needed: `mise self-update`).
2. Add `"pypi:hf" = "latest"` to `dot_config/mise/config.toml`.
3. Delete `.chezmoiscripts/run_once_5_aitools_4hf_install.sh` (and its commit),
   since mise now owns the install.
4. `cz apply` to verify.

<!-- verification notes
- v2026.9.3 `mise backends ls` → no `pypi` (confirmed 2026-10-12).
- `["pypi:hf"]` on v2026.9.3 → `pypi is not a valid plugin name`.
- `mise exec -- uv tool install hf` works on v2026.9.3 → hf==2.2.0 at
  ~/.local/bin/hf (this is what the run_once script does; the workaround).
- pypi backend first commit 2026-09-13/#13130; shipped in v2026.9.7.
-->
