# Persistent zellij session on cyberkleiber

Every interactive login on cyberkleiber (local console **or** ssh) drops you into
the **same** zellij session. The session survives ssh disconnects and is
resurrected across reboots, so you always land back in the same workspace with
the same panes in the same project directories.

## How it works

Three pieces cooperate:

1. **`~/.config/zellij/config.kdl`** — `session_serialization true`. This is the
   switch that makes zellij serialize each session (tabs, panes, running
   commands, **and each pane's working directory**) to disk. Without it, a
   killed/rebooted session is just gone.

2. **`~/.config/fish/conf.d/zellij.fish`** — on every interactive fish shell it
   runs `zellij attach -c persistent` (create-if-absent, then attach). Because
   the session name is fixed (`persistent`) and serialization is on, reconnecting
   always finds the same session. The snippet also detaches/exits cleanly so
   closing the window drops the ssh connection but keeps the session alive.

3. **`~/.local/bin/zattach.sh`** — manual helper for terminals that don't run
   this fish config, or when you want to reset:
   - `zattach.sh` — attach to (or create) the persistent session
   - `zattach.sh new` — kill the persistent session and start it fresh
   - `zattach.sh list` — list existing zellij sessions

## Working directories are preserved per pane

Each pane's cwd is stored individually in the serialized layout, so after a
reboot every pane re-opens in its own project directory — panes do **not**
collapse onto a single directory.

Serialization lives at:

```
~/.cache/zellij/contract_version_1/session_info/<session-name>/session-layout.kdl
```

Example (each pane keeps its own cwd):

```kdl
pane command="pi" cwd="home/jan/.local/share/chezmoi/devices/hp_z6_g4" { ... }
pane command="sleep" name="sleep 3600" cwd="tmp/aaaaaaaa" { ... }
```

On `zellij attach -c persistent`, zellij re-reads this file and starts each
pane's shell (fish `-lic`) in the recorded directory — the same idea as
`pi --continue` resuming in the working directory.

## Verifying

```bash
zellij action list-panes                              # titles show project per pi pane
cat ~/.cache/zellij/contract_version_1/session_info/persistent/session-layout.kdl
```

## Notes / gotchas

- The first time, only sessions created *after* `session_serialization true` are
  fully serialized; going forward everything persists.
- Stale sessions show as `EXITED - attach to resurrect` in `zellij list-sessions`.
  They are harmless — you only ever attach by the `persistent` name.
- `zattach.sh` is rendered from `dot_local/bin/executable_zattach.sh`; chezmoi
  strips the `executable_` prefix but keeps the `.sh` suffix (repo convention).
- The persistent session currently holds the different projects as **panes**
  (e.g. `π - chezmoi`, `π - hp_z6_g4`, `π - discoveries`) inside the one
  `persistent` session. If you'd rather have one session per project, the single
  `persistent` auto-attach name is what you'd change — see below.

## Open question: overlaps

If closing/detaching or reconnecting makes two clients fight for focus, or if a
resurrected pane re-runs `pi --continue` and spawns a second agent in the same
dir, the fix is to make resurrected panes open a bare `fish -lic` (just drop
into the saved cwd) instead of re-invoking `pi --continue`. Flag this and the
fish config can be adjusted.
