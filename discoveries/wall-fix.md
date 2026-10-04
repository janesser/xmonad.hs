---
title: Fixing `wall` (broadcast-to-all-terminals) on cyberkleiber
status: implemented
created: 2026-10-03
updated: 2026-10-03
---

# Fixing `wall` on cyberkleiber

> **Status:** **IMPLEMENTED (2026-10-03)**. Approach C (fish banner) is built,
> tested, deployed to `~/.local/bin/wall` + `~/.config/fish/conf.d/wall.fish`,
> and committed (`15bc4ba`). See §8 for the implementation notes.
>
> **One-line problem:** `wall` (write a message to all login terminals)
> delivers nothing on this box. `wall` exits 0 but reaches nobody.

## 1. Purpose

Make a `wall`-style broadcast actually visible on the interactive terminals
this user runs — SSH shell and zellij panes — on the cyberkleiber workstation.

## 2. Why it doesn't work today

`wall` / `who` / `write` only deliver to terminals recorded in **`/run/utmp`**
(the utmp database). On this box utmp is empty: `who` shows only
`system boot`, and no process writes active login rows.

Root cause is **structural**, not a config typo (all verified live):

- **No utmp writer exists** on the real rootfs:
  - `systemd-utmcd` (systemd ≥ 256 helper that replaced Ubuntu's `pam_utmp.so`)
    — **no binary, no unit file, no package owns it.**
  - `pam_utmp.so` — **absent** (Ubuntu dropped it in favor of utmcd; confirmed
    not in `/usr/lib/x86_64-linux-gnu/pam/`).
  - `pam_exec.so` — **absent** too, so no PAM-hook workaround.
  - `UTMP_FILE` unset in `login.defs`; `/run/utmp` and `/var/run/utmp` don't exist.
- The systemd here is a **minimal/container variant**: `/lib/systemd/systemd`
  is only ~141 KB (real systemd is several MB); PID 1 is under `/init.scope`.
- `sudo -n` **requires a password** — the chezmoi NOPASSWD drop-in does not cover
  a blanket `sudo`, so privileged steps (create `/run/utmp` writable, add `jan`
  to the `utmp` group, install/enable a writer) cannot be done automatically.

Secondary confirmation: `systemd-logind` (pid running) *does* exist, but every
interactive `jan` session shows **TTY `-`** (no controlling terminal), so there
is no login Session with a tty to map into utmp anyway.

This is the documented behavior of this exact release, not a local
misconfiguration — see §9 (Chris Siebenmann, "Ubuntu 26.04 has broken shutdown
announcements and `wall` doesn't work"): Ubuntu 26.04/systemd 259.5 ships
without `/run/utmp` (so `wall` does nothing) and logind doesn't learn SSH login
ptys (so `loginctl` reports `TTY -`). cyberkleiber matches all of this.

## 3. The zellij connection (why utmp alone never fixed this)

This is the decisive architectural point. `wall` writes to a tty **slave**;
whoever holds the **master** side reads the bytes into that process's stdin.

- `pts/0` (mode `0620`, group `tty`) = the SSH terminal your client sees → owned
  by the **zellij server/client** process tree (not a login process).
- `pts/1–4` (mode `0600`, `jan:jan`) = **zellij pane ptys** — internal to
  zellij, never created by a login.
- All masters are held by zellij, **not by the fish inside them**.

Therefore:
- `utmcd` only writes rows for **logind Sessions with a controlling TTY**.
  zellij's ptys are **not** logind sessions → utmcd would never register them,
  even if it were installed.
- Even a "fixed" utmp row on `pts/0` wouldn't help: zellij consumes `pts/0` as
  **control input**, not screen text, and has **no `intercept`/broadcast hook**
  (unlike tmux). A `wall` write to `pts/0` is swallowed, not rendered.

So `wall`/utmp cannot reach zellij panes by design. This is why the fix has to
move off the tty.

## 4. Approaches considered (with status)

### A. systemd-utmcd / `pam_utmp` — NOT FEASIBLE
- utmcd absent, `pam_utmp` absent, needs root to enable/install. Neither can be
  done under the chezmoi sudo boundary. **Dropped.**

### B. Zellij-native broadcast — PARTIALLY FEASIBLE, but only for *input*, not display
- **Sync mode** (`toggle-active-sync-tab`, your `Ctrl-s`): mirrors keystrokes to
  all panes on the **current tab**. Good for running the same command live across
  panes; **not** a one-shot visible banner; only covers one tab.
- **Scripted per-pane**: `zellij list-panes --json` +
  `zellij action write` / `paste` / `send-keys --pane-id <id> …`. Writes to pane
  **STDIN** (what the running program sees). Not a visible banner.
- **Existing community plugin `atani/zellij-send-keys`**: the tmux `send-keys`
  equivalent — `send-to-pane <id> "command"` + `list_panes`; works outside a
  session via `ZELLIJ_SESSION_NAME`. **It writes to pane STDIN** (granted via a
  permission dialog) → for *running commands everywhere*, not a display/wall.
  Install = drop `.wasm` into `~/.config/zellij/plugins/` + grant permissions.
- **No existing plugin renders a visible overlay/banner.** A true `wall` banner
  needs a custom plugin that draws an overlay (zellij pipes broadcast by default
  and plugins listen via the `pipe` lifecycle — so it's buildable, nothing
  prominent ships today).

### C. Fish-layer banner (RECOMMENDED / IMPLEMENTED 2026-10-03) — feasible, no root, works inside zellij
Route through a store that **fish itself surfaces** at its prompt instead of
through the tty:
1. `~/.local/bin/wall` writes the message to a shared, user-writable store:
   `~/.local/share/wall/inbox/<id>/body` (one dir per message, `id` = nanosecond
   epoch so names sort chronologically) + `~/.local/share/wall/latest` (newest
   id). No utmp, no root, no zellij involvement.
2. Each fish prints any messages newer than the ones this pane already shown
   (tracked per-pane in `~/.local/share/wall/seen/<fish_pid>`) as a banner above
   its prompt on the next `fish_prompt`, then records the newest id it showed.
3. Each pane's own fish renders the banner → every pane shows it. Works inside
   zellij. Fullychezmoi-managed (`~/.local/bin/wall` +
   `~/.config/fish/conf.d/wall.fish`), lands across boxes with `cz apply`.
- **Per-pane, once-per-message:** each pane is a separate fish process, so each
  shows every message exactly once (on its next prompt) and only *new* messages
  after that. A brand-new pane catches up on the backlog; a live pane stays
  silent once it has shown everything. Verified end-to-end.
- **Tradeoff:** fish is single-threaded → **prompt-gated**, not a live push.
  Banner appears the next time a prompt is drawn (after a command / on new
  input). Fine for announcements (like MOTD-per-pane); not a real-time pop.
- Composes with the existing `kitty-shell-integration.fish`: `wall.fish`
  **prepends** to any existing `fish_prompt` (never replaces it).
- `wall` subcommands: post (or `wall < file` for stdin), `history [--limit N]`,
  `clean [--older-than N]` (also prunes stale `seen/<pid>` files), `reset`.

### D. Direct tty write to fish — NOT FEASIBLE
- A literal `wall` to a fish's tty lands the bytes in fish's **stdin**, where
  fish would try to *execute* the message as a command — wrong behavior.
- And in zellij the master is held by zellij, so fish never receives it. **Dropped.**

## 5. Verification state (2026-10-03 — implemented + verified)
- Multi-pane behaviour verified end-to-end against the installed `wall` + a real
  fish process: pane A shows a posted message once, stays silent on its next
  prompt, then shows only the next *new* message; a brand-new pane (separate
  process) catches up on the whole backlog. Each pane is its own fish process, so
  per-pane seen-state lives in `~/.local/share/wall/seen/<fish_pid>`.
- `bash -n` clean; all subcommands (`post`/stdin, `history`, `clean`, `reset`)
  exit 0; truncation (`WALL_MAX_LEN`, default 4000), multi-line stdin bodies,
  and `clean` seen-pruning all exercised.
- Box facts (still accurate): systemd-utmcd/pam_utmp/pam_exec absent, systemd
  ~141 KB stub, logind running but jan sessions show `TTY -`, sudo needs a
  password, fish 4.9.3. `cz apply` is currently blocked on this box by an
  out-of-sync archive (23 `R` renames — pre-existing, unrelated), so the files
  were deployed directly (`install`) and committed instead; see §8.1.

## 6. Status — no open build decisions
Approach C is implemented, committed, and deployed. The remaining options are
now user-preference calls, not blockers:

- **Run a command in every pane (not a banner):** the tmux `send-keys` path,
  where `zellij-send-keys` is the ready-made plugin. Note this writes to pane
  STDIN (what the running program sees), not a visible banner.
- **A live push instead of prompt-gated:** only a custom zellij overlay plugin
  would give that; the fish banner is prompt-gated by nature.
- **Scope:** currently every interactive fish (console + ssh + zellij panes).
  The pi-agent's non-tty shells are skipped by `status is-command-substitution`.

## 7. Implementation notes
- **`~/.local/bin/wall`** (`dot_local/bin/executable_wall`, bash): posts to
  `~/.local/share/wall/inbox/<id>/body` (`id` = `date +%s%N`, one dir per
  message so names sort chronologically) and writes the newest id to
  `~/.local/share/wall/latest`. `history [--limit N]` lists newest-first,
  `clean [--older-than N]` purges inbox dirs and stale `seen/<pid>` files,
  `reset` wipes the store. Body is single-line-trimmed; messages over
  `WALL_MAX_LEN` (default 4000) are truncated with a marker. No root, no utmp.
- **`~/.config/fish/conf.d/wall.fish`** (`dot_config/private_fish/conf.d/wall.fish`):
  on each `fish_prompt` it prints any inbox ids newer than this pane's
  `seen/<fish_pid>` marker, then advances the marker. Prepends to any existing
  `fish_prompt` (kitty integration untouched). Guards
  `status is-command-substitution` so command substitution never sees the banner.
- **Gotcha fixed here:** `string trim < file` inside `$(...)` *inside an `if`*
  returns empty on this fish (the redirect silently no-ops in a compound block),
  so file reads use `cat`; and `set --local` inside an `if` shadows the
  function-local, so the seen-marker uses a bare `set` that mutates it.

## 8. Where the state is recorded
- Failure memory (target=`failure`, category=`insight`): the utmp/utmcd absence
  + zellij-ptys-are-not-logind-sessions + wall-can't-reach-zellij-via-utmcd
  chain, including the sudo-boundary note. **Status in that memory is now
  stale** — it reads "never fixed"; it is fixed (Approach C). Consider updating
  it to "implemented via fish banner, cz-apply blocked by archive sync".
- git: `15bc4ba` `Add wall pane broadcast + fish banner for zellij`.
- Sessions: 2026-10-01 ~21:47–22:05 (root-cause), 2026-10-03 (implementation).

## 9. Related reading — Ubuntu 26.04 shutdown and `wall`

- Reference: https://utcc.utoronto.ca/~cks/space/blog/linux/Ubuntu2604ShutdownAndWall
  (Chris Siebenmann, 2026-07-20, "Ubuntu 26.04 has broken shutdown announcements
  and `wall` doesn't work"). The site blocks AI agents, so the summary below is
  from the downloaded HTML of the actual article, not a guess.
- **The article's two separate issues.**
  1. **`wall` is broken on 26.04.** Starting in Debian 13 ('Trixie') and
     Ubuntu 25.10, Debian/Ubuntu systemd is built **without support for
     `/run/utmp`**. `wall` (from `bsdutils`) only looks in the utmp file, so
     with no utmp file, `wall` never does anything. The recommended fix is to
     "write a script that gets the list of active user sessions with ptys and
     writes a message to them itself." The utmp replacement is the `wtmpdb`
     package (hooked via PAM), but Ubuntu 26.04's OpenSSH is built without
     wtmpdb support.
  2. **Shutdown broadcasts are broken on 26.04.** systemd **259.5**'s logind
     does not learn which ttys SSH logins use, so `loginctl` shows **TTY `-`**
     for every SSH session (console logins still get a TTY). logind only
     announces impending shutdowns to TTYs it knows about, so SSH ptys get
     nothing. This does *not* happen on Ubuntu 24.04 (systemd 255.4) or Fedora
     43 (systemd 258.9); Fedora 44 (systemd 259.7, UTMP re-enabled) is fine.
- **This is exactly our box.** cyberkleiber is **Ubuntu 26.04.1, systemd
  259.5**, has **no `/run/utmp`**, and every `jan` session shows **`TTY -`** in
  `loginctl` (verified 2026-10-05). So §2's root cause is not a local anomaly —
  it is the documented behavior of this release. The `who`/`w` programs still
  show pty info by falling back to scanning `/dev/pts` (not via systemd/utmp),
  and AppArmor's `who` profile additionally blocks `/run/systemd/sessions`.
- **Consequences for this project.**
  1. **Root cause fully confirmed, direction unchanged.** Our finding that
     `/run/utmp` is empty and logind reports `TTY -` is the published behavior
     of Ubuntu 26.04, not a misconfiguration. A utmp/utmcd-based fix was never
     viable here; the OS simply ships without utmp for `wall`.
  2. **Approach C is precisely the article's recommended vector.** The article
     says the fix is "get the active ptys and write to them yourself" — i.e.
     bypass `wall`/utmp entirely. Our store + per-pane fish banner is the
     fish-native realization of that. It is the correct approach and it is the
     one the article points to.
  3. **Caveat the article implies: a raw `wall`-replacement won't reach
     zellij panes.** Writing to the ptys found via `/dev/pts` reaches raw SSH
     and console sessions (nothing between the write and the screen), but zellij
     holds the *master* side of a pane's pty, so bytes written to the slave are
     consumed by zellij, not rendered. That is exactly why the banner has to be
     fish rendering the store, not an external write to the tty — the one place
     our approach is strictly better than the article's suggested `wall` clone.
  4. **Immediacy is still a fish limit, independent of `wall`.** Even reaching
     the store, the banner shows only on the next prompt draw (fish is
     single-threaded with no file-watch/IPC hook; a `--on-signal` handler was
     verified not to fire while idle). True instant delivery still means
     rendering outside fish (zellij overlay/plugin) — see §6.
  5. **Practical notes for any future tooling here.** Do not use `wall`, `who`,
     or `loginctl` for session/pty discovery on this release: utmp is gone and
     logind doesn't track SSH ptys. If you must discover ptys, scan `/dev/pts`
     (or `who`/`w`, which fall back to `/dev/pts`, subject to AppArmor), and
     know that `wtmpdb` won't help without a PAM hook and OpenSSH rebuild.
  6. **Shutdown notices still work via the store.** A halt oneshot can post the
     shutdown message into the inbox, so panes render it on their next prompt
     after reboot — even though neither `wall` nor logind will announce it.
