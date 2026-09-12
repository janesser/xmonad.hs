# AGENTS.md — jan's chezmoi dotfiles

Personal chezmoi dotfiles repo (xmonad / X11 Linux desktop). Managed via
`chezmoi` (alias `cz`). GitHub: `janesser/xmonad.hs.git`. Human guide: `README.md`.

## pi-agent's sudo boundary (important)
pi-agent MAY run `cz update` (chezmoi pull + apply). Privileged steps run under a
scoped NOPASSWD sudoers drop-in at `/etc/sudoers.d/chezmoi-pi`, which grants root
**only** for the exact command families chezmoi run scripts use:

`apt, add-apt-repository, nala, snap, usermod, groupadd, systemctl,
update-alternatives, mkdir, chmod, chown, tee, cp, gpg, mv, sed, extrepo,
dpkg, curl, journalctl`, plus direct `chmod`/`chown` on the podman.sock path (the
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
`/etc/systemd/system/restart-llama-server.service` (`WantedBy=multi-user.target`).
Because it is a system unit, no `loginctl enable-linger` is required.

- Provisioned by `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`, which
  installs the unit and adds the huggingface-hub bind-mount to `/etc/fstab`
  (`bind,nofail`) so the hub is available without passwordless `sudo mount`.
- Everything runs under the scoped NOPASSWD sudoers drop-in (`install`, `tee`,
  `cp`, `systemctl`) — see the sudo boundary above.
- Lifecycle: `systemctl --system enable --now restart-llama-server`,
  `journalctl --system -u restart-llama-server -f`.

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
