# AGENTS.md — jan's chezmoi dotfiles

Personal chezmoi dotfiles repo (xmonad / X11 Linux desktop). Managed via
`chezmoi` (alias `cz`). GitHub: `janesser/xmonad.hs.git`. Human guide: `README.md`.

The `README.md` is the human guide — for topics not covered in this AGENTS.md
(e.g. the full sudoers setup, package lists), it is the source of truth.

## Tool usage: every bash command needs a timeout
Run **every** `bash` tool call with an explicit `timeout`, without exception —
even quick, obviously-fast commands. There is no default timeout, so omitting
it leaves a hung process (slow apt/snap/network step, a stalled `cz`, a
waiting consent prompt) to block the session indefinitely. Pick a sane value for
the command's nature (seconds for local greps/edits; minutes for apt/snap/`cz
update`). This rule overrides any general "omit timeout unless needed" guidance.

## Agent directives: every system change via chezmoi

**Mandatory rule for the agent:** every system change must be managed through
chezmoi — never edit a file directly in its target location
(`~/.config/...`, `/etc/...`, `~/.local/...`, `~/.ssh/...`, etc.). This is a
strong recommendation for **every** system adjustment, not an option.

- **Create in place, then `chezmoi add`.** Write the file exactly where it
  belongs and add it with `cz add <destination>` so it is tracked in the
  source tree. Do **not** create it elsewhere and copy it over, and never
  leave a managed file untracked.
- **Encrypt what the repo must not see.** If a file is too revealing for the
  public GitHub repo, add it with `cz add --encrypt <destination>` (or
  `cz encrypt <dest>` then `cz re-add --encrypt <dest>` for a file already
  managed, refreshed with `cz re-add --encrypt`). When in doubt, encrypt.
  See the [Encryption](#encryption-age-encrypted--concealed-files) section
  for the full rules.
- **Ask before fixing pre-commit hooks.** If a pre-commit hook reports a
  false positive (e.g. `betterleaks`/`foxguard` flagging a file it should
  ignore), **STOP and ask me** before running any `cz` command to fix it.
  Do **not** blindly re-run with `--no-verify`, `--force`, or any other
  bypass.

## Dotfile naming conventions (destination mapping)

chezmoi renders the source tree to the home directory by convention — the
source name **is** the destination path. Know this mapping before creating
anything:

| Source prefix / tree | Destination | Example |
|---|---|---|
| `dot_<name>` | `~/.<name>` | `dot_bashrc` → `~/.bashrc` |
| `dot_config/<app>` | `~/.config/<app>` | `dot_config/btop` → `~/.config/btop` |
| `private_dot_<name>` | `~/.<name>` (private) | `private_dot_ssh` → `~/.ssh` |
| `executable_<name>` | `~/.local/bin/<name>` | `executable_zattach.sh` → `~/.local/bin/zattach.sh` |
| `etc/<path>` | `/etc/<path>` | `etc/fuse.conf` → `/etc/fuse.conf` |
| `usr/<path>` | `/usr/<path>` | `usr/share/...` |
| `commons/` | shared (no destination) | scripts used by run scripts |
| `devices/<host>/` | host-specific notes | not rendered |

Consequences:
- `dot_local/bin/executable_<name>.sh` lands as `~/.local/bin/<name>.sh`;
  `.fish` lands as `~/.local/bin/<name>.fish` — the extension is part of the
  destination name, not a chezmoi convention.
- A file that must not be committed (secrets, host-specific keys) goes under
  `dot_pi/` or gets `cz add --encrypt` — see [Encryption](#encryption-age-encrypted--concealed-files).

### Source code references

These conventions are defined in chezmoi's source:

- Prefixes/suffixes constants: [internal/chezmoi/chezmoi.go](https://github.com/twpayne/chezmoi/blob/master/internal/chezmoi/chezmoi.go)
- Attribute parsing logic: [internal/chezmoi/attr.go](https://github.com/twpayne/chezmoi/blob/master/internal/chezmoi/attr.go)
- Full reference: [Source state attributes](https://www.chezmoi.io/reference/source-state-attributes/)

## `cz` command reference

| Command | Use |
|---|---|
| `cz add <dest>` | track a new file at its destination |
| `cz add --encrypt <dest>` | track + age-encrypt (public repo) |
| `cz re-add <dest>` | refresh an already-tracked file |
| `cz re-add --encrypt <dest>` | refresh + encrypt |
| `cz apply` | render the dotfile set to the system |
| `cz update` | pull upstream + apply |
| `cz diff` | show pending changes vs. the rendered set |
| `cz status` | list tracked-but-unrendered files |
| `cz remove <dest>` | untrack a file (keeps the rendered copy) |
| `cz encrypt <src>` / `cz decrypt <src>` | convert between `.age` and cleartext |
| `cz init` | re-render the runtime config (`chezmoi.toml`) |

Commands are documented at `chezmoi.io/reference/commands/<command>/` — e.g.
`add`, `re-add`, `apply`, `update`, `diff`, `status`, `remove`, `encrypt`, `decrypt`,
`init`.

## Repo layout

**Externals:** `xmonad` and `xmonad-contrib` under `.config/xmonad/` are
git-repo externals managed by `.chezmoiexternal.toml` (refreshed every 168h).
They are **not** source to edit — `cz apply` clones/refreshes them automatically.

**Ignored trees:** `etc/` and `usr/` are in `.chezmoiignore` — they are
rendered into the system by run scripts, not by chezmoi. `discoveries/`,
`devices/`, and `uninstaller/` are also ignored; they are working notes and
dev tools, never part of the rendered dotfile set.

**Fleet:** `raspberrypi` (this box, apt; home-assistant + pacman-repo). `flowerpower` = `omarchy` = `chuwi_ubook_xpro` (pacman, camera-debug stack). `cyberkleiber` = `hp_z6_g4` (memory hub, ai-server). `jetson_nano_b210` is broken for now, planned restore. The `devices/` boxes (`rpi400`, `lincopta` = `xmg-core-15-amd-m20`, `foss_android`) are dev-only hosts with host-specific notes.

## pi-agent's sudo boundary (important)
pi-agent MAY run `cz update` (chezmoi pull + apply). Privileged steps run under a
scoped NOPASSWD sudoers drop-ins under `/etc/sudoers.d/`, each granting root
**only** for the exact command families chezmoi run scripts use. Every box
gets `chezmoi-pi-common` (journalctl, systemctl, the file-write commands);
apt/nala/snap boxes additionally get `chezmoi-pi`, pacman/yay boxes get
`chezmoi-pi-pacman`; the camera-debug box gets `chezmoi-pi-camera-debug`.

The per-box drop-in grants the package-manager family plus what that box
needs (e.g. `setcap cap_perfmon+ep /usr/bin/btop` on the apt box with an
Intel GPU). Cleanup scripts only ever use `apt`/`rm`/`install`, so they run
under the package-manager family **without a new sudoers entry** — don't add
one just because a cleanup script calls `sudo apt`/`sudo rm`. The TeamViewer
uninstaller at `uninstaller/teamviewer_uninstall.sh` is an example: it uses
only those.

**Don't be explicit about sudoers in repo files.** Point to this AGENTS.md
section instead of documenting sudo **authorization** specifics inline. In
particular, files must not spell out *which commands the alias authorizes*,
alias names (`CHEZMOI_PKGS`), or "no new sudoers entry needed" — that is
operational security detail that lives here, not in the repo. Inline **may**
note the *functional* reason `sudo` is used (e.g. why `install` over `cp`)
and may briefly say sudo is scoped to the drop-in documented here; it must
not audit the sudoers authorization or enumerate authorized commands. This
applies repo-wide. Exceptions: the sudoers source files under `etc/sudoers.d/`
and the install script `run_onchange_sudo_sudoers_chezmoi_pi.sh.tmpl`, whose
path comments describe the file they manage.

Rules:
- Do **not** use `sudo` for anything outside a chezmoi run script.
- If a needed command is **not** in the sudoers alias, STOP and ask the user —
  do not try to work around the boundary.
- Never edit `/etc/sudoers.d/chezmoi-pi` or broaden any NOPASSWD rule without
  explicit user approval.
- Prefer `cz apply` for config edits; reserve `cz update` for pulling fresh
  upstream changes.
- **`git force push` (`--force`, `--force-with-lease`, `--force-with-lease=force`,
  or any other force-push) is always prohibited.** It is unsafe on a shared
  remote branch because it overwrites history other people have already pulled.
  Never run it, and never treat a "prepare" request as permission to run it —
  stage the change and refuse the force-push; ask the user to run it by hand
  if they are the only writer and truly need it.
- When the request says **"prepare"** an action, it is *staged only*: set it
  up (stage/commit, run the checks, print what would happen) and then STOP.
  Never auto-execute a destructive, irreversible, or networked action
  (`rm -rf`, `reset`/`rebase`, `shutdown`) just because it was "prepared" —
  the user wanted it ready to fire, not fired. Confirm before applying.

## Sudoers files (repo-managed under `etc/`)

Three drop-ins, split so each box gets only the rules it can use, plus one
manual install (camera-debug box only):

| Drop-in | Box flavour | Install script |
|---|---|---|
| `etc/sudoers.d/chezmoi-pi-common` | **all** boxes | `run_onchange_..._chezmoi_pi_common.sh.tmpl` |
| `etc/sudoers.d/chezmoi-pi` | apt / nala / snap | `run_onchange_..._chezmoi_pi.sh.tmpl` |
| `etc/sudoers.d/chezmoi-pi-pacman` | pacman / yay | `run_onchange_..._chezmoi_pi_pacman.sh.tmpl` |

`etc/sudoers.d/chezmoi-pi-camera-debug` is installed manually, only on the
Chuwi box.

Each drop-in is installed (with an interactive yes/no confirmation) by its
`run_onchange` script as part of `cz update`. Each script embeds a SHA256 of its
own source file, so chezmoi only runs it when that source file changes — on
every other apply it skips silently, with no `sudo` and no prompt. All three
scripts are safe to run on the whole fleet; each one also skips on the wrong
box flavour (apt needs `apt`/`nala`, pacman needs `pacman`).

Manual fallback + validation: the "pi-agent scoped sudo for `cz update`"
section of the top-level README.md.

The old `pi/` scaffold has been removed — the sudoers files now live in the
natural `etc/` tree, so `pi/` is obsolete.

## Encryption (age-encrypted / concealed files)

Some files are **age-encrypted** and stored in the source tree as `.age` with the
`encrypted_` filename prefix. They never appear in the repo in cleartext.

Encryption is documented in chezmoi's docs — [encryption](https://www.chezmoi.io/user-guide/encryption/) and
the [age](https://github.com/FiloSottile/age) implementation it uses.

**Current count: 44 `.age` files** (`find . -name '*.age' | grep -v .chezmoistate`),
growing as more secrets are added — refresh this figure when it changes.

- **Method:** `encryption = "age"` in `~/.config/chezmoi/chezmai.toml`. Recipients
  are listed there (labels: `peacewagon`, `lincopta`, `no5`, `cyberkleiber`).
- **Identity = the only decryption key:** `~/.config/chezmoi/age-id.txt`. It is
  **local-only** (not chezmai-managed, not in the repo). **Back it up separately**
  — without it all 44 `.age` files are unrecoverable even if you have the
  recipients.
- **Notable encrypted destinations (source blobs under `dot_pi/` etc.):**
  - pi-agent: `~/.pi/agent/settings.json`, `~/.pi/agent/crossbar.json`
    (`dot_pi/agent/encrypted_{settings.json,crossbar.json}.age`)
  - SSH: `~/.ssh/config` (`private_dot_ssh/encrypted_config.age`)
  - git: `~/.gitconfig` (`encrypted_dot_gitconfig.age`)
  - Evolution mail/addressbooks/calendar/RSS: `~/.config/evolution/*` (~36 files,
    the bulk of the count)
  - App configs: KeeXc (`~/.config/keepassxc/keepassxc.ini`), meteo-qt, Nextcloud
  - Host-specific: `~/.local/bin/wakeup` on cyberkleiber
    (`dot_local/bin/encrypted_executable_wakeup.cyberkleiber.age`)
- **Add a new encrypted file:** `cz add --encrypt <destination>` (one-shot), or
  `cz encrypt <dest>` then `cz re-add --encrypt <dest>`; an already-managed file
  is refreshed with `cz re-add --encrypt <dest>`. When you add a recipient for a
  new host, add it to `chezmai.toml` **first**, then re-encrypt affected files
  (`cz re-add --encrypt`), or they won't decrypt on that host.
- **Don't** `cz diff`/edit/`cz apply` a `.age` expecting cleartext — read it with
  `cz decrypt <source>`. A `.age` blob is opaque in a diff.
- **Distinguish:** `etc/apt/keyrings/teamviewer-keyring.gpg` is a plain **public**
  keyring committed in the clear — **not** concealed. It is not one of the `.age`
  files.

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

### Mutual support protocol — RETIRED (2026-10-23)

The llama-cuda → llama-sycl handover (marker + executor + caller-resume +
return, via `llama-handover-request.sh` / `llama-handover.sh`, event log under
`~/.local/share/llama-cpp/handover/`) is retired: this box no longer has an
Intel GPU (the SYCL backend was a crash-looping zombie with no device), and
the little brother's text backend on card 2 (`llama-little-brother`, :8082 —
CUDA, not SYCL) will provide a permanent second text backend. Design and the
full retirement list: `discoveries/ai-server/little-brother-card2-swap.md`.
The event log is kept at `~/.local/share/llama-cpp/handover/handover.log` as
an audit trail. **Until the card-2 backend is deployed, stopping `llama-cuda`
leaves no local text backend — plan around that.**
## `dot_local/bin` scripts: the `.sh` suffix

Scripts under `dot_local/bin/` render to `~/.local/bin/` with the same
extension the source keeps — the extension is part of the destination name
(`executable_<name>.sh` → `~/.local/bin/<name>.sh`, `executable_<name>.fish`
→ `~/.local/bin/<name>.fish`); for the full mapping see the [naming
conventions](#dotfile-naming-conventions-destination-mapping) section.

The `.sh` suffix is **intentional** — jan wants scripts visually distinguishable
from everything else in `~/.local/bin/` (e.g. in `dmenu`).

Consequences:
- The command is `zattach.sh`, not `zattach`. The script's own usage text
  says `zattach`, but the rendered command is `zattach.sh`. Do **not** rename
  the source to `executable_zattach` to get a bare `zattach` unless jan
  explicitly asks — the suffix is the point.
- When adding a new standalone script here, keep the `.sh` extension
  (`executable_<name>.sh`), matching this convention.
- The same rule applies to `dot_local/bin` executables that are actually
  symlinks (`symlink_<name>` → no destination extension).

## Discoveries (`discoveries/`)

`discoveries/` holds research, proposals, and handover notes for things jan is
exploring or has explored. It is **not** managed into the system — these are
working notes.

### Project status — shared memory (the rollup is a live query)

The static `discoveries-status.yaml` is **retired** (pi-hermes-memory v2,
FR-11): project status lives in the shared LAN memory on cyberkleiber, and
the rollup is a **live query** — the latest status entry per project by
`updated_at` — never a file that must be kept in sync. There is no YAML, no
`MAP.md`, no summary to bump.

**When a project changes state, update the status fact in shared memory in
the same change** — with the pi-hermes-memory `memory_status` tool
(registered when this box is enrolled in the shared memory):

- Set: `memory_status { project, state, content }` — `state` is one of the
  six words below; `content` is a short note (why it changed / what is next).
  It is visible from every box immediately, with machine + session provenance.
- Query: `memory_status { project }` — current state, note, last state
  change, and which machine made it. Works on a box WITHOUT the project
  checked out (resolution goes through the registry).

Trigger events:

- a project starting or resuming active work (`active`)
- a project verified but with a concrete next step remaining (`hardening`)
- a project finishing — work verified / handed over (`done`)
- a project being parked with no imminent action (`held`)
- a project superseded or with no planned follow-up (`dormant`)
- a project explicitly killed in favour of something else (`abandoned`)

When the hub (cyberkleiber) is down the tool says so explicitly: a query
reports live status unavailable, and a set lands in the local project memory
(visible on this box only) — re-issue the set when the hub is back if it must
be shared.

### Status buckets

(The six words live in the shared vocabulary of pi-hermes-memory —
`STATUS_WORDS` — and in the `memory_status` tool schema. Human-facing meaning:
`active` → working on it; `hardening` → works, but a concrete next step remains;
`done` → verified, no pending work; `held` → parked, no imminent action;
`dormant` → research note, superseded; `abandoned` → explicitly killed.)

