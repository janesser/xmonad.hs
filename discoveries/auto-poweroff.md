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

