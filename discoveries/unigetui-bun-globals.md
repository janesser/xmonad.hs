# Handover: UniGetUI does not list installed Bun global packages

**Date:** 2026
**Tool:** `unigetui` = **UniGetUI** (Devolutions) — desktop GUI for WinGet/Scoop/Chocolatey/Pip/Npm/.NET Tool/PowerShell Gallery.
**Symptom:** UniGetUI finds Bun as a package manager but the "Installed packages" list for Bun is empty, even though `bun pm -g list` reports ~161 installed global packages.

---

## Environment findings

- Bun is installed via **Scoop**, version `1.4.2`.
  - Real binary: `C:\Users\jesse\scoop\apps\bun\current\bun.exe`
  - Exposed through shim: `C:\Users\jesse\scoop\shims\bun.exe` (already on User PATH).
- Bun is a **portable** UniGetUI install; settings live in:
  `C:\Users\jesse\scoop\apps\unigetui\current\Settings\`
- `BUN_INSTALL` env var is **set** to `C:\Users\jesse\scoop\persist\bun`.
- Bun's real global packages directory:
  `C:\Users\jesse\scoop\persist\bun\install\global\node_modules`
  (161 packages, incl. `@earendil-works/pi-coding-agent@0.85.1`).
- Default Bun location `C:\Users\jesse\.bun\install\global` **does not exist** (only a `cache` folder under `C:\Users\jesse\.bun\install`).

## Settings read

`Settings\Configuration\DisabledManagers.json`:
```json
{ "Bun": false, "Pip": true }
```
- `Bun: false` → Bun is **enabled** (this is why UniGetUI "found" bun).
- `Pip: true` → Pip is **disabled**.

`Settings\InstallationOptions\GlobalValues.Bun.json` → `{}` (no custom Bun values).

## Root cause

UniGetUI's Bun manager (`Bun.cs` → `GetGlobalPackagesDirectory()`) **hardcodes**
`%USERPROFILE%\.bun\install\global` and **ignores the `BUN_INSTALL` env var** that bun itself uses.
Because bun here resolves its home to `C:\Users\jesse\scoop\persist\bun`, the hardcoded path is empty,
so UniGetUI lists **0 installed bun packages**.

This is the known bug **Devolutions/UniGetUI#4918** — "GetGlobalPackagesDirectory ignores BUN_INSTALL".

---

## Fix

Create a **junction** so UniGetUI's hardcoded `%USERPROFILE%\.bun` resolves to the real Scoop-persist bun folder.
Bun keeps using `BUN_INSTALL`; UniGetUI gets a valid path. No change to bun's config needed.

```powershell
$real = "C:\Users\jesse\scoop\persist\bun"
$link = "C:\Users\jesse\.bun"
if (-not (Test-Path $link)) {
  New-Item -ItemType Junction -Value $real -Path $link | Out-Null
  Write-Output "Created junction: $link -> $real"
} else {
  Write-Output "$link already exists:"; Get-Item $link | Select-Object FullName,Target
}
Test-Path (Join-Path $link "install\global")   # expect True
```

Then **fully close and reopen UniGetUI** (don't just refresh) so it rescans. It should now list the ~161 installed bun global packages.

### Alternatives
- **Upgrade UniGetUI** (`scoop update unigetui`) if a newer build fixed the `BUN_INSTALL` handling — currently on `2026.2.7`.
- **Remove `BUN_INSTALL`** so bun falls back to the default `~/.bun` location — changes bun's existing setup; not recommended just for the UI.

---

## Verification
- `bun pm -g list` → shows the global node_modules dir with installed packages (already working).
- After the junction + restart, UniGetUI's Bun "Installed" tab should show the packages; check the "updates" view too (that path was also affected by the same bug).

## Open / follow-ups
- If a later UniGetUI release fixes issue #4918, the junction becomes optional (but harmless to keep).
- Confirm bun's own behavior is unaffected by the `.bun` junction (it should be transparent since it already targets the same tree via `BUN_INSTALL`).
