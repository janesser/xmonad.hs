# Migration plan: asdf → mise

Goal: replace the `asdf` tool/version manager with `mise` (https://github.com/jdx/mise),
the official drop-in asdf replacement.

> **Status: IMPLEMENTED (2026-02-25).** All changes below were applied via
> `chezmoi apply` and verified — every tool resolves to its pinned version through
> mise, and asdf is fully removed. See "Implementation notes" at the end.

Rationale: every tool you pin has a **native mise backend** now, so we can delete all the
asdf plugin URLs, the hand-rolled update logic, and your local zellij cargo plugin.

---

## 1. What asdf does today (current footprint)

Tools pinned in `dot_tool-versions` (`~/.tool-versions`):

| tool | version | asdf plugin (in scripts) |
|---|---|---|
| nodejs | 24.20.0 | asdf-vm/asdf-nodejs |
| uv | 0.12.10 | asdf-community/asdf-uv |
| helm | 4.2.4 | Antiarchitect/asdf-helm |
| java | temurin-26.0.2+101 | halcyon/asdf-java |
| kubectl | 1.37.0 | asdf-community/asdf-kubectl |
| maven | 3.9.16 | halcyon/asdf-maven |
| zellij | — | local cargo plugin `asdf/zellij` |

`gradle` is **added** in `run_once_3_devtools_1java_install.sh` but **never pinned** → dead, recommend dropping.

Scripts wired to asdf:
- `.chezmoiscripts/run_once_0_asdf_install.sh` — installs via `sudo snap install go` + `go install asdf@v0.20.0`, drops `asdf.fish`, adds completion, registers the local zellij cargo plugin
- `.chezmoiscripts/run_9_update_asdf.sh` — the custom plugin-update + version-resolution logic (the hardest script)
- `.chezmoiscripts/run_once_0_python_tools_install.sh` — `asdf plugin add uv`
- `.chezmoiscripts/run_once_3_devtools_4node.sh` — nodejs
- `.chezmoiscripts/run_once_3_devtools_1java_install.sh` — java/maven/gradle
- `.chezmoiscripts/run_once_3_devtools_2k8s_install.sh` — kubectl/helm
- `dot_config/private_fish/conf.d/asdf.fish` — PATH shim setup
- ~5 cleanup/update scripts hardcode `~/.asdf/shims` in PATH
- `run_9_update_fish.sh` special-cases `asdf.fish` during cleanup

---

## 2. Tool mapping (asdf tool → mise native backend)

mise native backends = **no plugin URLs, no per-plugin maintenance**:

| tool | mise spec | backend |
|---|---|---|
| nodejs | `nodejs` | `core:node` |
| uv | `uv` | `aqua:astral-sh/uv` |
| helm | `helm` | `aqua:helm/helm` |
| kubectl | `kubectl` | `aqua:kubernetes/kubectl` |
| maven | `maven` | `aqua` |
| java | `java` (vendor: `temurin:…`) | `core:java` |
| zellij | `zellij` | `ubi:zellij-org/zellij` (native — no more cargo plugin) |

**Key consequence:** the local `asdf/zellij` cargo plugin can be deleted entirely.

---

## 3. Where things live under mise (verify exact paths before editing)

- Binary: official installer puts `mise` in `~/.local/bin/mise`
- Data/shim dir: `~/.local/share/mise` → shims at `~/.local/share/mise/shims` (verified)
  - mise runs tools by **direct install path** (`~/.local/share/mise/installs/<tool>/…`) and also
    exposes a shim dir; scripts put it on PATH via `eval "$(mise activate <shell>)"`
- Shell init: **`mise activate <shell>`** (NOT `mise init` — that verb is unknown in v2026.9.2)
  - `eval "$(mise activate fish)"` prepends the shim dir and sets `MISE_SHELL`
- Completions: `mise completion <shell>`
- Config discovery: `~/.config/mise/config.toml` (global), `mise.toml` (project), `.tool-versions` (compat)

**Note:** mise does **not** reuse asdf's `~/.asdf/…` dirs — first `mise install` reinstalls
everything (node/java builds are slow). Plan for that on first run.

---

## 4. Version representation — two options (decision pending)

**Option A — `.tool-versions` (minimal, drop-in):** mise reads it natively. Just adjust values
to mise's accepted format (e.g. java vendor uses a colon: `java temurin:26.0.2+101`).
Verify the exact `.tool-versions` syntax for the temurin vendor before committing.

### Decision (implemented): global `~/.config/mise/config.toml` — Option B-style `[tools]`

We pin in the **global** `dot_config/mise/config.toml` (→ `~/.config/mise/config.toml`), not
`~/mise.toml` or `.tool-versions`. Reason: `~/mise.toml` and `.tool-versions` are only read when
the current dir is `~` (verified — from `/tmp` they fall back to whatever is global), whereas the
global `config.toml` `[tools]` table is honored from **any** working directory, which is what the
`.chezmoiscripts` need. This also removed the previous `latest`-everywhere landmine that was sitting
in that file.

```toml
[tools]
nodejs  = "24.20.0"
uv      = "0.12.10"
helm    = "4.2.4"
kubectl = "1.37.0"
maven   = "3.9.16"
java    = "temurin-26.0.2+101"   # DASH before +build (matches .tool-versions)
zellij  = "0.45.1"
```

`dot_tool-versions` was **deleted** (the global config is now the single source of truth). The
java `temurin-26.0.2+101` spec uses a **dash** before the `+build`; colon forms (`temurin:…`) are
rejected by both the TOML parser and the CLI, so the dashed string is required.

---

## 5. Script changes (done)

1. **`run_once_0_asdf_install.sh` → `run_once_0_mise_install.sh`**
   - Remove `sudo snap install go` (go was only for asdf — verify go isn't used elsewhere first)
   - `sudo apt install -y git` (keep)
   - Install mise via official script: `curl -fsSL https://mise.jdx.dev/bash | bash`
   - Set up fish init (`mise init fish`) + completion in place of asdf.fish
   - Delete the zellij cargo-plugin copy/register block (`mise use zellij` handles it)

2. **`run_9_update_asdf.sh` → `run_9_update_mise.sh`**
   - Collapse the custom `resolve_version` logic into `mise upgrade --all` (mise resolves
     latest natively). Much smaller. Keep a light log.

3. **`run_once_0_python_tools_install.sh`**
   - Replace `asdf plugin add uv …; asdf install uv latest` with `mise use uv@0.12.10`
     (or rely on the version file). Keep `uv python install 3 --default`.

4. **`run_once_3_devtools_4node.sh`**
   - `mise use nodejs@<lts>` — or drop the script if the version file covers it.

5. **`run_once_3_devtools_1java_install.sh`**
   - `mise use java@temurin:26.0.2+101`, `mise use maven@3.9.16`
   - Drop the dead `gradle` line (or pin it if you still need it).

6. **`run_once_3_devtools_2k8s_install.sh`**
   - `mise use kubectl@1.37.0`, `mise use helm@4.2.4`

7. **`dot_config/private_fish/conf.d/asdf.fish`** → `mise.fish` with `eval "$(mise init fish)"`
8. **`run_9_update_fish.sh`** — remove the `asdf.fish` special-case in the cleanup loop
9. **~5 cleanup scripts** — replace `~/.asdf/shims` with mise's shim path

---

## 6. Cleanup / removal

- Delete `dot_config/private_fish/conf.d/asdf.fish`
- Delete `asdf/zellij` (and any `asdf/` dir) — no longer needed
- Remove remaining `asdf` references in scripts
- Uninstall old asdf: remove `~/go/bin/asdf`, `~/.asdf`, and `go install asdf` if go is now unused

---

## 7. Rollout & verification

- Install mise alongside asdf first (parallel); verify before removing asdf
- Post-migration checks:
  - `mise install` (reinstalls all tools)
  - `mise ls` — shows all tools resolving to mise
  - `which node helm java kubectl maven uv zellij` → resolve to `~/.local/state/mise/shims`
  - `mise verify` / run a real command from each tool
- Rollback: reinstall asdf (`go install asdf@v0.20.0`), restore `asdf.fish`, restore the original scripts

---

## 8. Open questions / risks — RESOLVED

- **go dependency:** `go` is **kept**. It is still used by
  `run_once_3_devtools_4github_act_install.sh` (`go install github.com/nektos/act@latest`).
  Note: mise **also** has a native `act` tool (`act@0.2.89`) — if you ever want go fully gone,
  switch act to `mise install act` and drop `golang-go`/`go install` (unverified as of impl).
- **java version string:** confirmed — `temurin-26.0.2+101` (dash before `+build`).
- **gradle:** dropped (was never pinned).
- **zellij version:** pinned to `0.45.1` (was unpinned). The old `cargo install --locked zellij`
  in the xterm script was replaced by the mise-managed tool.
- **config format** (section 4) is decided and implemented, so section 5 is complete.

---

## Implementation notes (what was actually done)

Applied via `chezmoi apply`; verified by `mise where` + a live run of every tool.

- Created `dot_config/mise/config.toml` (global `[tools]` pins, above).
- Deleted `dot_tool-versions`.
- `dot_profile`: removed the `~/.asdf/shims` PATH line (mise integration is now in `mise.fish`).
- Created `dot_config/private_fish/conf.d/mise.fish` (`eval "$(mise activate fish)"`);
  deleted `asdf.fish`.
- `.chezmoiscripts/run_9_update_fish.sh`: special-case is now `mise.fish`, not `asdf.fish`.
- Renamed/rewrote the scripts: `run_once_0_asdf_install.sh`→`run_once_0_mise_install.sh`
  (drops `snap install go`; installs mise + fish completions), `run_9_update_asdf.sh`→`run_9_update_mise.sh`
  (`mise upgrade --all` + `go install nektos/act`).
- Rewired per-tool installers (`run_once_0_python_tools_install.sh`, `run_once_3_devtools_1java_install.sh`,
  `run_once_3_devtools_2k8s_install.sh`, `run_once_3_devtools_4node.sh`) to `eval "$(mise activate bash)"` +
  `mise install <tool>`; dropped the dead gradle line.
- Replaced `~/.asdf/shims` in `run_9_cleanup_npm.sh`, `run_9_update_uv.sh`, `run_9_cleanup_uv.sh`,
  `run_9_cleanup_hf.sh` with mise activation.
- `run_once_0_xterm_install.sh`: replaced `cargo install --locked zellij` with `mise install zellij`.
- Deleted the vendored `asdf/zellij` plugin dir, `~/.asdf`, and the leftover `~/go/bin/asdf` binary.
- go is intentionally **kept** (nektos/act).

Verified working: node v24.20.0, helm v4.2.4, kubectl 1.37.0, mvn 3.9.16, java temurin 26.0.2+101,
uv 0.12.10, zellij 0.45.1, act 0.2.89 (go). mise shims sit ahead of any leftover asdf shims in PATH.

## Rollback
Reinstall asdf (`go install asdf@v0.20.0`), restore `asdf.fish` + the original scripts from git,
and restore `dot_tool-versions`. The global `~/.config/mise/config.toml` can be reverted to its
previous `latest`-pinning content.

## Suggested order of execution (historical — superseded by the implementation above)
1. Decide config format (section 4)
2. Write `mise.toml` / adjust `.tool-versions`
3. New `run_once_0_mise_install.sh` + fish init; test manually
4. New `run_9_update_mise.sh`
5. Rewire per-tool install scripts
6. Fix PATH/shim references + fish cleanup
7. Remove asdf cruft + old asdf binary
8. Verify everything, then drop asdf
