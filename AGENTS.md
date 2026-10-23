# AGENTS.md — jan's chezmoi dotfiles

Personal chezmoi dotfiles repo (xmonad / X11 Linux desktop). Managed via
`chezmoi` (alias `cz`). GitHub: `janesser/xmonad.hs.git`. Human guide: `README.md`.

## Tool usage: every bash command needs a timeout
Run **every** `bash` tool call with an explicit `timeout`, without exception —
even quick, obviously-fast commands. There is no default timeout, so omitting
it leaves a hung process (slow apt/snap/network step, a stalled `cz`, a
waiting consent prompt) to block the session indefinitely. Pick a sane value for
the command's nature (seconds for local greps/edits; minutes for apt/snap/`cz
update`). This rule overrides any general "omit timeout unless needed" guidance.

## pi-agent's sudo boundary (important)
pi-agent MAY run `cz update` (chezmoi pull + apply). Privileged steps run under a
scoped NOPASSWD sudoers drop-in at `/etc/sudoers.d/chezmoi-pi`, which grants root
**only** for the exact command families chezmoi run scripts use:

`apt, add-apt-repository, nala, snap, usermod, groupadd, systemctl,
update-alternatives, mkdir, chmod, chown, tee, cp, gpg, mv, sed, extrepo,
dpkg, curl, rm, install, cmp, journalctl, setcap cap_perfmon+ep /usr/bin/btop`, plus direct `chmod`/`chown` on the podman.sock path (the
`CHEZMOI_PKGS` alias authorizes `/usr/bin/chmod`/`/usr/bin/chown` with any
args; the shell glob is expanded before sudo, so no wildcard reaches it).

Cleanup scripts only ever use `apt`/`rm`/`install`, so they run under this
same alias **without a new sudoers entry** — don't add one just because a
cleanup script calls `sudo apt`/`sudo rm`. The TeamViewer uninstaller at
`uninstaller/teamviewer_uninstall.sh` is an example: it uses only those.

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

## Encryption (age-encrypted / concealed files)

Some files are **age-encrypted** and stored in the source tree as `.age` with the
`encrypted_` filename prefix. They never appear in the repo in cleartext.
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

### Mutual support protocol: llama-cuda → llama-sycl handover (cyberkleiber ONLY)

On **cyberkleiber** the single Tesla V100 is owned by `llama-cuda` (:8081, CUDA;
~27 GB ornith), while `llama-sycl` (:8082, Intel Iris Xe) is the steady
companion and does **not** use the V100. Whenever `llama-cuda` is **unloaded /
stopped** on this box, serving must be handed to `llama-sycl` so an inference
backend stays available. This is **mandatory** — do not stop `llama-cuda` on
cyberkleiber without first recording the handover, and complete the handover
before finishing the session that caused the stop.

It is coordinated by **two files** (triggered by a single `pi-command` — step 1
below):

- Request marker: `~/.local/share/llama-cpp/handover/requested` (JSON, one event).
- Event log:      `~/.local/share/llama-cpp/handover/handover.log` (append-only).

**1. Record the stop (mandatory trigger).** When you stop/unload `llama-cuda`
on cyberkleiber, write the marker right away with the helper
`~/.local/bin/llama-handover-request.sh`. This is the **only** step that runs
`pi` and names an explicit `--provider` (the :8082 sycl provider). `PI_SESSION_ID`
is the pi session id — it is stamped on both files so the handover is always
attributable. The helper auto-picks the handover model (first id from
`curl :8082/v1/models`) and embeds the ready-to-run command as `pi_command` in
the marker. **Calling it also triggers step 2 — the marker write is what runs
the executor:**

```bash
~/.local/bin/llama-handover-request.sh "stop" "systemctl stop llama-cuda"
```

**2. Execute the handover (executor — a result of step 1's call).** Writing the
marker triggers `~/.local/bin/llama-handover.sh`. **Precondition:**
`llama-sycl` (:8082) is already up. The executor owns tearing down `llama-cuda`
(the caller never runs `systemctl stop llama-cuda` itself). It reads the marker,
stops `llama-cuda` (:8081), verifies :8081 is down, confirms :8082 answers, and
logs the event — **leaves the marker in place** so a resumed caller can read the
ready `pi_command`. It never runs `pi --provider`. In order:

1. No `~/.local/share/llama-cpp/handover/requested` → print “nothing to do” and
   exit 0.
2. Stop `llama-cuda` (`sudo systemctl stop llama-cuda`) and verify :8081 is
   down (`curl :8081/v1/models` must fail). If :8081 is still up, abort — aborts
   keep the marker and log an `EVENT=aborted` (retryable).
3. Assume `llama-sycl` (:8082) is already up — just confirm it answers
   (`curl :8082/v1/models`); do NOT wait-start here (it is a boot unit). If :8082
   is not reachable, abort (marker kept).
4. Complete the ready `pi_command` the resumed caller will run. The model is
   normally pre-picked by llama-handover-request.sh (embedded in the marker as
   `pi_command`/`handover_model`); fill it in here only if sycl was down at
   request time. Print it for convenience — the marker is deliberately **not**
   cleared so the caller can read the command from it.
5. Append a `completed` log line. The marker is **left in place** (cleared only
   on return/cancel):
   ```
   <ISO8601> | SESSION=$PI_SESSION_ID | EVENT=completed | :8082 serving; pi_command=<as in marker>
   ```

**3. Caller resumes — read the handover file (human-triggered).** After the
executor frees the V100, the caller reads `pi_command` from the marker at the
predefined location `~/.local/share/llama-cpp/handover/requested` — the spot
where the caller-agent expects to retrieve the handover outcome — and runs it.
This is the sole `pi --provider` in the flow (the command embedded by step 1):

```bash
eval "$(python3 -c 'import json;print(json.load(open("$HOME/.local/share/llama-cpp/handover/requested"))["pi_command"])')"
```

**4. Return the handover (llama-cuda comes back).** When `llama-cuda` is
started again, complete the reverse and clear the marker. `defaultProvider` is
never used or changed — the :8082 `--provider` only ever lived in step 1, so pi
is simply back on llama-cuda (:8081):

```bash
sudo systemctl restart llama-cuda          # :8081 back up (see stop test below)
MODEL=$(curl -fsS http://127.0.0.1:8081/v1/models \
  | python3 -c 'import json,sys;print(json.load(sys.stdin)["data"][0]["id"])')
pi --model "$MODEL"
echo "$(date --iso-8601=seconds) | SESSION=${PI_SESSION_ID:-unknown} | EVENT=restored | llama-cuda back on :8081" >> "$HOME/.local/share/llama-cpp/handover/handover.log"
rm -f "$HOME/.local/share/llama-cpp/handover/requested"
```

Boot note: both `llama-cuda` and `llama-sycl` are boot units
(`WantedBy=multi-user.target`) and come back up on their own, so after a wake
the handed-over workload is already serving on :8082 and `llama-cuda` is back on
:8081 — nothing to do by hand. The only manual case is an **interrupted
handover** — if the marker is still present on wake (the executor did not finish
before power-off), just re-run `llama-handover.sh`; it sees the pending marker and
completes (or aborts cleanly) the handover.

**Event-log schema** (one line per event, append-only):
`<ISO8601> | SESSION=<pi-session-id> | EVENT=<requested|completed|restored|cancelled> | <human note>`.

**Guards.**
- This protocol is **cyberkleiber-only** (the only V100 + Iris Xe box). On any
  other host, stopping `llama-cuda` does **not** trigger a handover.
- Never stop `llama-cuda` on cyberkleiber without first writing the marker —
  an untracked stop leaves no audit trail and no handover is guaranteed.
- `llama-cuda` is now `Type=exec` (reworked to `Type=exec`, see below), so
  `systemctl stop` genuinely kills :8081 and `is_active` is reliable — verify
  `:8081` is down in step 2 before trusting the marker.
- The `pi --provider` appears **only in step 1's embedded `pi_command`** (per
  invocation override — never writes `~/.pi/agent/settings.json`). Steps 2
  (executor), 3 (caller resume) and 4 (return) never set a `--provider` of
  their own, and `defaultProvider` is never used or changed at any point. The
  handover model is auto-picked (first id from `curl :8082/v1/models`).

**Pending decision (open — do not treat as final).** The handover is currently
SPLIT across two scripts + the caller: `llama-handover-request.sh` writes the
marker **and triggers the executor** (`llama-handover.sh`); the executor stops
`llama-cuda` + confirms :8082 (leaving the marker intact) so the resumed caller
reads `pi_command` from the marker and runs it; the return (restart
`llama-cuda`) clears the marker. Whether to MERGE this into fewer script(s) —
e.g. one
`llama-handover.sh` that owns `stop cuda → confirm sycl`, paired with a single
`up cuda → hand back` command — is UNRESOLVED. Keep the current split until jan
decides; do not refactor these scripts until this flag is cleared.

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

## `dot_local/bin` scripts: keep the `.sh` suffix

Scripts under `dot_local/bin/` keep their `.sh` extension through chezmoi's
render, so they land in `~/.local/bin/` as `zattach.sh`, `brightness.sh`,
`checkip.sh`, etc. (not bare `zattach`). This is **intentional** — jan wants
the `.sh` suffix so scripts are visually distinguishable from everything else
in `~/.local/bin/` (e.g. in `dmenu`).

Consequences:
- The command is `zattach.sh`, not `zattach`. The script's own usage text
  says `zattach`, but the rendered command is `zattach.sh`. Do **not** rename
  the source to `executable_zattach` to get a bare `zattach` unless jan
  explicitly asks — the suffix is the point.
- When adding a new standalone script here, keep the `.sh` extension
  (`executable_<name>.sh`), matching this convention.

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
`STATUS_WORDS` — and in the `memory_status` tool schema; this list is the
human-facing meaning.)

- **hardening** — works, but still needs hardening before it is fully done:
  a concrete next step remains (cleanup, a reproducibility re-run, edge cases,
  robustness). Introduced 2026-10-12, positioned between `active` and `done`.
- **done** — implemented/verified, no pending work (may still be referenced as
  handover).
- **active** — actively worked on; has a concrete next step.
- **held** — proposal or in-flight with no imminent next action; parked, not
  abandoned.
- **dormant** — research/reference note, superseded, or no planned follow-up.
- **abandoned** — explicitly killed in favour of something else.

