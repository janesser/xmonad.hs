# AGENTS.md — jan's chezmoi dotfiles

Personal chezmoi dotfiles repo (xmonad / X11 Linux desktop). Managed via
`chezmoi` (alias `cz`). GitHub: `janesser/xmonad.hs.git`. Human guide: `README.md`.

## pi-agent's sudo boundary (important)
pi-agent MAY run `cz update` (chezmoi pull + apply). Privileged steps run under a
scoped NOPASSWD sudoers drop-in at `/etc/sudoers.d/chezmoi-pi`, which grants root
**only** for the exact command families chezmoi run scripts use:

`apt, add-apt-repository, nala, snap, usermod, groupadd, systemctl,
update-alternatives, mkdir, chmod, chown, tee, cp, gpg, mv, sed, extrepo,
dpkg, curl, journalctl, setcap cap_perfmon+ep /usr/bin/btop`, plus direct `chmod`/`chown` on the podman.sock path (the
`CHEZMOI_PKGS` alias authorizes `/usr/bin/chmod`/`/usr/bin/chown` with any
args; the shell glob is expanded before sudo, so no wildcard reaches it).

Rules:
- Do **not** use `sudo` for anything outside a chezmoi run script.
- If a needed command is **not** in the sudoers alias, STOP and ask the user —
  do not try to work around the boundary.
- Never edit `/etc/sudoers.d/chezmoi-pi` or broaden any NOPASSWD rule without
  explicit user approval.
- Prefer `cz apply` for config edits; reserve `cz update` for pulling fresh
  upstream changes.

## Sudoers file (repo-managed under `etc/`)
Source: `etc/sudoers.d/chezmoi-pi` (a normal chezmoi file, but `etc/` is in
`.chezmoiignore`, so it is never auto-applied to `/etc`). Installed, with an
interactive yes/no confirmation, by the chezmoi **`run_onchange`** script
`.chezmoiscripts/run_onchange_9_0_sudoers_chezmoi_pi.sh.tmpl` as part of `cz
update`. The script embeds a SHA256 of the sudoers source file, so chezmoi only
runs it when that source file changes (skipped — no `sudo`, no prompt — on
every other apply).
Manual fallback + validation: the "pi-agent scoped sudo for `cz update`" section
of the top-level README.md.

The old `pi/` scaffold has been removed — the sudoers file now lives in the
natural `etc/` tree, so `pi/` is obsolete.

## Backend: llama.cpp server at boot

The llama.cpp server is a **backend** service, not a user dotfile. It is started
at every boot (before any login) by a *system* systemd unit —
`/etc/systemd/system/llama-cuda.service` (`WantedBy=multi-user.target`).
Because it is a system unit, no `loginctl enable-linger` is required.

- Provisioned by `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`, which
  installs the unit and adds the huggingface-hub bind-mount to `/etc/fstab`
  (`bind,nofail`) so the hub is available without passwordless `sudo mount`.
- Everything runs under the scoped NOPASSWD sudoers drop-in (`install`, `tee`,
  `cp`, `systemctl`) — see the sudo boundary above.
- Lifecycle: `systemctl --system enable --now llama-cuda`,
  `journalctl --system -u llama-cuda -f`.

## btop: dual GPU (Iiris Xe + NVIDIA) visibility

btop only shows the NVIDIA box for the Intel Iris Xe / DG1 (gpu1). This is a
**btop bug fixed by patching + rebuilding btop**, not a config issue.

- **Root cause (btop 1.4.6):** `src/linux/intel_gpu_top/intel_gpu_top.c`
  hardcodes `const char* device = "i915"`. Recent kernels instantiate the i915
  perf PMU by PCI address (`/sys/devices/i915_0000_2f_00.0/events`), so
  `discover_engines()` scans the missing `/sys/devices/i915/events` and
  `pmu_init()` reads the missing `/sys/bus/event_source/devices/i915/type` → the
  Iris Xe (gpu1) silently vanishes. V100 (gpu0, NVML) is unaffected.
- **Fix (patched):** `discover_engines()` scans `/sys/devices` for an
  `i915*` dir exposing `events/` and reassigns `engines->device` to the resolved
  instance name (e.g. `i915_0000_2f_00.0`) so `pmu_init()` finds the perf
  event source type. `free()` added on both the `err` path and `free_engines()`.
  On DG1 / IGPUs (no RAPL energy-gpu PMU) power + temperature are read from the
  i915 hwmon sensor (cumulative `energy1` microjoule counter + `temp1`) and
  instantaneous power is derived from the perf-sampled energy delta in
  `btop_collect` (see `src/linux/btop_collect.cpp`).
- **`CAP_PERFMON` also required:** `perf_event_open()` on the i915 PMU needs
  `cap_perfmon` on the binary (`kernel.perf_event_paranoid = 4` blocks it). The
  patched binary lives at `~/.local/bin/btop` (user-owned), so the cap is
  granted with `sudo setcap cap_perfmon+ep ~/.local/bin/btop` — this needs
  jan's sudo **password**, since the scoped NOPASSWD setcap rule only ever named
  `/usr/bin/btop` (now the restored vanilla package). Don't broaden the sudoers
  to cover the local path (see the sudo boundary).
- **Deployed:** `~/.local/bin/btop` = `1.4.7+2ad5308`, `cap_perfmon=ep`,
  `jan:jan` — built from the committed fix in `~/projs/btop` (its current HEAD).
  `/usr/bin/btop` is the restored vanilla `1.4.6-2` apt package (left untouched).
- **Dev clone (MR dev):** `~/projs/btop` — a **full** clone on branch
  `btop-intel-gpu-fix` with the fix committed; develop and inspect here, and
  push the branch to `janesser` when the fix advances. It is **not** used to
  build (the deploy uses a separate clean clone — see below). Full clone
  preferred over the previous shallow one so an MR can be built locally.
- **Deploy run script:**
  `.chezmoiscripts/run_once_5_aitools_3btop_intel_gpu_cap.sh` (run_once,
  **tracked in git**). **Policy: patched btop only where an Intel GPU exists;
  every other host keeps the vanilla system btop untouched.** Two layers enforce
  this — a top-level `has_intel_gpu` guard (log + exit 0), plus a defence-in-depth
  assertion inside `ensure_btop()` that refuses to install if `has_intel_gpu` is
  false. For reproducibility the build uses a **dedicated clean clone** at
  `~/projs/btop-intel-gpu` of the fork's `btop-intel-gpu-fix` branch — never the
  developer's working clone (which may be mid-rebase) and never upstream
  aristocratos/btop (which has no fix). On a fresh box it clones the fork over
  SSH (HTTPS fallback); it keeps the clone current with `git fetch` + checkout
  and rebuilds only when the fork branch advances (stamp at
  `~/.local/share/btop_deployed_commit`). Installs to `~/.local/bin/btop` (no
  root). The `cap_perfmon` grant is the only sudo step and, targeting the
  user-local path, needs jan's password (not the NOPASSWD drop-in). `cz update`
  needs network only to fetch the fork (or the existing clone). (Dry-run both
  branches by hiding/lspci to test the non-Intel path.)
- **Config:** `~/.config/btop/btop.conf` (`shown_gpus = "nvidia amd intel"`,
  `shown_boxes = "cpu mem net proc gpu0 gpu1"`) — tracked by chezmoi; both GPUs
  render as separate boxes once the patched binary + cap are in place.
- **Verification (headless, no live TUI):** run with `--debug` and confirm the
  log has **no** `Failed to find Intel GPU engines` and **no**
  `Failed to initialize PMU` (success is silent in btop). Sysfs proof: the
  `i915_*` dir exposes `events/` and
  `/sys/bus/event_source/devices/i915_0000_2f_00.0/type` exists.
- **Sudo boundary:** the `setcap` line already lives at the `CHEZMOI_PKGS`
  alias (AGENTS.md line 13: `/usr/sbin/setcap cap_perfmon+ep /usr/bin/btop`,
  exact match — sudoers forbids intra-arg wildcards). `setcap` is in
  `/usr/sbin`, not `/usr/bin`.

## Auto poweroff at scheduled times

Another root-run **system** systemd unit (timer + oneshot service) that powers
the machine off at configured local times, installed to `/etc/systemd/system`
(no login required, like the llama service).

- Inputs (plain files — edit then `cz apply`):
  `etc/systemd/system/auto-poweroff.times` (one `HH:MM` per line; midnight =
  `00:00`) and `etc/systemd/system/auto-poweroff.delay` (grace seconds, 0 =
  immediate). A non-zero delay schedules a *cancellable* shutdown,
  cancelable with `sudo shutdown -c`.
- Gated by the `auto_poweroff` toggle in the runtime config
  (`~/.config/chezmoi/chezmoi.toml`, rendered from `.chezmoi.toml.tmpl`). It
  defaults to `false` and is `true` only on this machine via
  `.chezmoi.hostname`. Edit `.chezmoi.toml.tmpl`, then `cz init` (re-renders
  the config) **then** `cz apply` (the run script reads the toggle). The
  change-hash also includes `.chezmoi.toml.tmpl`, so flipping the toggle
  re-runs the script.
- Deployed + enabled by `.chezmoiscripts/run_onchange_9_1_auto_poweroff_timer.sh.tmpl`
  (change-gated via an embedded inputs hash). Launcher is
  `/usr/local/bin/auto-poweroff.sh`.
- Uses only sudo commands in the scoped drop-in (`mkdir`, `cp`, `chmod`,
  `chown`, `systemctl`).
- Lifecycle: `systemctl --system enable --now auto-poweroff.timer`,
  `systemctl list-timers auto-poweroff.timer --all`,
  `journalctl -t auto-poweroff`.
