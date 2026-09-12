# chezmoi on Windows

chezmoi already handles most Windows things for you — you mainly use template
conditionals on `.chezmoi.os` / `.chezmoi.system` plus Windows-specific path
functions.

## OS / system detection
```gotemplate
{{ if eq .chezmoi.os "windows" }}   ... {{ end }}
{{ if eq .chezmoi.system "windows" }} ... {{ end }}
{{ .chezmoi.windowsVersion }}       // e.g. "10" / "11"
```
WSL detection (runs under WSL on Windows):
```gotemplate
{{ if and (eq .chezmoi.os "linux") (contains "microsoft" (.chezmoi.kernel.osrelease | lower)) }}
  # WSL-specific code
{{ end }}
```

## Path helpers (the most important Windows bits)
| Thing | Equivalent |
|---|---|
| `joinPath` | `{{ joinPath .chezmoi.homeDir "AppData" "x" }}` — uses OS separator automatically |
| path separator | `.chezmoi.pathSeparator` → `\` on Windows, `/` elsewhere |
| path *list* separator | `.chezmoi.pathListSeparator` → `;` on Windows, `:` elsewhere (for `PATH`, etc.) |
| home dir | `.chezmoi.homeDir` (use forward slashes in templates; chezmoi normalizes) |
| AppData location | `C:\Users\<me>\AppData\...` — build with `joinPath .chezmoi.homeDir "AppData"` |

## Installing on Windows
```
scoop install chezmoi
choco install chezmoi
winget install twpayne.chezmoi
```

## Executing commands (PowerShell / cmd)
chezmoi's `exec` template function runs whatever's on `PATH`:
```gotemplate
{{ if exec "test" "-d" (joinPath .chezmoi.homeDir "scoop") }}
  scoop-aware block
{{ end }}
```
For Windows you'll typically drive **PowerShell**:
```gotemplate
{{ exec "powershell" "-NoProfile" "-Command" "..." }}
```

## Scripts (`.chezmoiscripts/`)
Same as your Linux repo, but:
- Use `.sh.tmpl` under WSL/Git-Bash, **or** `.ps1.tmpl` for native Windows.
- `run_once_*` is how you bootstrap packages/tools on Windows too.
- Note: native Windows shell is `cmd`, so prefer PowerShell scripts there.

## Directory-based templating for per-OS targets
Because files are templates when the name ends in `.tmpl` or lives in
`.chezmoitemplates/`, you can point the same source file at different
Windows/unix destinations:
```gotemplate
# .chezmoi.directory.tmpl
{{ if eq .chezmoi.os "windows" }} {{ joinPath .chezmoi.homeDir "AppData" "Roaming" "nushell" }} {{ else }} {{ joinPath .chezmoi.homeDir ".config" "nushell" }} {{ end }}
```

## Practical gotchas for a Windows machine
1. **Backslash paths** in `.gitignore`/regexes need escaping (`\\`).
2. Prefer `.chezmoi.toml.tmpl` runtime config + `.chezmoi.hostname` toggles
   (like your Linux `auto_poweroff` pattern) — same mechanism works.
3. On *native* Windows (not WSL), executables are `.exe`/`.ps1`; test
   availability with the `exec` function before calling them.
4. Line endings: keep files as LF in git; chezmoi writes them correctly per-OS.
