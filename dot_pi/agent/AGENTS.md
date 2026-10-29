# AGENTS.md — jan's home-directory map (pi global instructions)

These instructions are the **user-level** agent directory file
(`<agent-dir>/AGENTS.md`, i.e. `~/.pi/agent/AGENTS.md`). They apply across
**all** working directories — unlike `~/.local/share/chezmoi/AGENTS.md`, which
is a project file that only loads when pi runs inside the chezmoi repo.

This is a compact map of the important paths in `~` and the evergreen,
cwd-independent facts you need before touching anything. Keep it focused on
*paths* and *boundaries*; repo-internal deploy minutiae belong in the repo's
own AGENTS.md, not here.

## Home-directory path map

| Path | What it is | Notes |
|---|---|---|
| `~/.pi/agent/` | **pi's own home** (agent dir). Do not manage with chezmoi. | `settings.json`, `models.json`, `auth.json`, `AGENTS.md`, `skills/`, `SESSIONS.md`. Persistent memory: `pi-hermes-memory/` (sessions.db + `skills/`). Conversations: `sessions/`. Packages in `settings.json.packages`. |
| `~/.local/share/chezmoi/` | **chezmoi repo** (source of truth). Alias `cz`. | Git: `janesser/xmonad.hs.git`. Files here are *source templates* that chezmoi renders to their home locations. `cz apply` = install; `cz update` = pull + apply. |
| `~/.config/` | Per-application config. | Rendered by chezmoi from `dot_config/`. e.g. `~/.config/chezmoi/chezmoi.toml` (runtime config), `~/.config/btop/btop.conf`. |
| `~/.local/bin/` | User local binaries (no root). | `~/.local/bin/btop` = patched 1.4.7 (Intel GPU fix, `cap_perfmon=ep`). System `btop` at `/usr/bin/btop` stays vanilla. |
| `~/.local/share/mise/` | mise toolchain installs. | Includes pi itself (`installs/pi/0.87.1/pi/...`). Docs: `.../pi/docs`. |
| `~/.cache/huggingface/hub/` | Local GGUF models for the llama.cpp backend. | `ornith.gguf` is the default model (`settings.json.defaultModel`). Bind-mounted into the llama service. |
| `~/projs/` | Personal dev clones (not dotfiles). | `~/projs/btop` = dev branch `btop-intel-gpu-fix`; `~/projs/btop-intel-gpu` = clean build clone used by the deploy script. |
| `~/.agents/skills/` | Standard user-level **Agent Skills** location (spec). | Empty today — skills here load in every cwd. Pi also reads `<agent-dir>/skills/` and project `.agents/skills/`. |

## bmad harness (project methodology)

BMAD-METHOD skills are installed as a **user-level (global) skill set** under
`~/.pi/agent/skills/bmad-*`. They are published there by the chezmoi run script
`.chezmoiscripts/run_onchange_pi_bmad.sh.tmpl`, which re-publishes the skills
whenever pi-agent could have changed (i.e. after any pi upgrade/reinstall, which
wipes `~/.pi`). The project-local `_bmad/` / `.agents/` install is obsolete —
`npx bmad-method install` is no longer run in the repo.

To actually run a BMAD session in a project you must install the runtime into
that project:
    npx bmad-method install --yes --modules bmm --tools pi \
        --directory <project-root> --action install --all-stable

The runtime's output (`_bmad-output/`) then lives in that project's root.

## Evergreen environment facts

- **pi-agent sudo boundary (important).** pi-agent may run `cz update`. Privileged
  steps run under the scoped NOPASSWD sudoers drop-in
  `/etc/sudoers.d/chezmoi-pi` (only `apt, add-apt-repository, nala, snap,
  usermod, groupadd, systemctl, update-alternatives, mkdir, chmod, chown, tee,
  cp, gpg, mv, sed, extrepo, dpkg, curl, journalctl, setcap .../usr/bin/btop`,
  plus direct chmod/chown on podman.sock). Do **not** sudo outside a chezmoi run
  script; if a needed command is missing, STOP and ask. Never edit the sudoers
  file. Prefer `cz apply`; reserve `cz update` for pulling upstream.
- **Backend: llama.cpp at boot.** System unit `/etc/systemd/system/restart-llama-
  server.service` (`WantedBy=multi-user.target`, no linger needed). Lifecycle:
  `systemctl --system enable --now restart-llama-server`.
- **Auto poweroff.** System timer + oneshot (`/etc/systemd/system/auto-poweroff.*`).
  Inputs: `etc/systemd/system/auto-poweroff.times` and `.../auto-poweroff.delay`
  (plain files — edit then `cz apply`). Cancel with `sudo shutdown -c`.
- **btop dual-GPU (Iris Xe + NVIDIA).** Iris Xe (gpu1) vanishes on btop 1.4.6 due
  to a hardcodes-`i915` perf-PMU bug. Fixed by a patched binary at
  `~/.local/bin/btop` (`cap_perfmon=ep`, user-owned) built from `~/projs/btop`.
  Needs jan's sudo **password** (the NOPASSWD setcap rule only named
  `/usr/bin/btop`). Do not broaden sudoers for the local path.
