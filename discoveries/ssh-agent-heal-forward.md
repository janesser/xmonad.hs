---
title: Healing a stale `SSH_AUTH_SOCK` (forwarded sshd agent)
status: implemented
created: 2026-10-10
updated: 2026-10-10
---

# Healing a stale `SSH_AUTH_SOCK`

> **Status:** **IMPLEMENTED (2026-10-10)** and left up to your testing. The
> tool is built, committed, and the calling shell heals in every mode. Next
> step is *your* test pass against real forwarded connections — see §6.
>
> **One-line problem:** a long-lived shell (or a frozen inherited env var)
> keeps `SSH_AUTH_SOCK` pointing at an sshd forwarding-agent socket that has
> since died, so every `ssh` / `git` / `scp` fails with
> "Error connecting to agent". The socket is gone but the variable is not
> updated, and nothing re-points it.

## 1. Purpose

Give a working, self-healing fix for a stale `SSH_AUTH_SOCK` so that any
subsequent command in the current shell (or a command launched from it) uses
the **live** sshd forwarding agent instead of the dead pointer.

## 2. Why it doesn't work today

- The sshd forwarding agent is **ephemeral**. Each agent-forwarded connection
  gets its own socket under `~/.ssh/agent/`, named `s.<connid>.sshd.<sessid>`
  (e.g. `s.iP1YJk5dGS.sshd.6CCD7VEqEz`). sshd removes the socket when the
  connection dies.
- A long-lived shell, or an env var inherited from a dead session, keeps the
  old path. The socket is gone → `ssh-add -l` and every ssh call fail.
- On this host there can be several sockets at once: dead ones (Connection
  refused) plus the currently live forwarded agent. Something has to **find the
  live one** among them.

## 3. Root cause of the hang (important)

`ssh-add -l` does **not** fail fast on a dead peer. A socket whose sshd side
died still accepts the Unix-socket connection, but the peer never replies, so
`ssh-add -l` **blocks** until a timeout. A naive "probe each socket with
`ssh-add -l`" therefore hangs on the first stale socket and never reaches the
live one. This is the trap the probe had to avoid — see §4.

## 4. The fix

Two artifacts, committed to chezmoi (source of truth):

1. **`dot_local/bin/executable_ssh-heal-forward.sh`** — standalone helper.
   - Probes the agent directory (`$SSH_AGENT_DIR` or `~/.ssh/agent/`) and the
     inherited `SSH_AUTH_SOCK`, and returns the first socket whose
     `ssh-add -l` **answers within 3s**. The 3s bound is what turns a hung
     stale socket into "not alive" so the probe moves on.
   - Modes (all verified):
     - no args → heals `SSH_AUTH_SOCK` in its own env, then `ssh-add -l`.
     - `<cmd…>` → same heal, then `exec "$@"` with the healed var.
     - `--which` → prints the live agent path only.
     - `--emit` → prints the correct directive for the *calling* shell:
       `export SSH_AUTH_SOCK="…"` (bash/zsh/ksh/dash) or
       `set -gx SSH_AUTH_SOCK "…"` (fish). Meant to be `eval`ed.
2. **`dot_config/private_fish/functions/ssh-heal-forward.fish`** — the fish
   **function** for the interactive shell. It runs *inside* the parent fish
   process, so it can `set -gx SSH_AUTH_SOCK …` directly and **needs no
   eval**. With no args it then `exec ssh-add -l`; with args it execs them.

Why two? A child process can never rewrite its parent's shell variables except
via `eval` of emitted output. A fish function avoids that entirely because it
runs in the parent. For any non-fish shell the eval one-liner
(`eval "$(ssh-heal-forward.sh --emit)"`) is the equivalent.

### Bugs hit while building (all fixed)

- `for … in ("$dir"/s.*.sshd.*)` — parens opened a subshell → syntax error.
  Glob must be unquoted: `for … in "$dir"/s.*.sshd.*`.
- `case` block missing `esac` → `done` flagged.
- `shift 2` with one arg → **infinite loop** (script hung). Use `shift`.
- `export SSH_AUTH_SOCK "…"` (space, not `=`) → "not a valid identifier".
- `exec "$@"` after the arg-parsing `shift` → nothing left to exec. Use
  `exec "${args[@]}"`.
- `$FISH_VERSION` unset under `set -u` → error. Use `${FISH_VERSION:-}`.
- §3 above — the probe needed a timeout or it hung forever.

## 5. How it is wired / deployed

- `cz apply` renders `dot_local/bin/…` → `~/.local/bin/ssh-heal-forward.sh`
  (chmod 755) and `dot_config/private_fish/…` →
  `~/.config/fish/functions/ssh-heal-forward.fish`.
- The fish function is tracked under `private_fish/` (not `dot_config/fish/`)
  on purpose: this repo already renders `.config/fish` from
  `dot_config/private_fish/`, and having both trees is a chezmoi
  "inconsistent state".
- Deployed copy on disk matches the committed sources (md5 verified).

## 6. Testing — left up to you

Verified modes (each exit 0 when a live agent exists):

- no-arg prints the live key list (`3072 SHA256:… jan@lincopta (RSA)`).
- `--which` / `--emit` / `--emit` + `eval` all re-point to the live socket.
- exec mode hands the healed `$SSH_AUTH_SOCK` to the child.
- no-agent case fails cleanly with a stderr message + exit 1 (not a hang).
- fish fn (with and without a command) heals the interactive shell.

**Please test against real forwarded connections**, specifically:

- the stale-socket scenario: point `SSH_AUTH_SOCK` at a dead `…sshd…` socket
  and confirm the probe skips it (≤3s each) and lands on the live one;
- a fresh `ssh -A` session and a command run immediately after;
- the fish fn at the fish prompt (`ssh-heal-forward`) with no args and with a
  command;
- a `bash -c` non-interactive case (harness) with `--emit` + eval.

Report back whether the worst-case probe latency (one stale socket × 3s) is
acceptable, and whether 3s is right or should be tuned. If a live agent is
ever slow to answer, that same 3s bound could misclassify it — worth a look.

## 7. Open questions / possible follow-ups

- **Auto-heal on every prompt.** Could attach the heal to the fish prompt
  (`--on-prompt`) and a bash `DEBUG` trap so `SSH_AUTH_SOCK` self-repairs
  continuously, since the forwarded agent is ephemeral. Not implemented —
  depends on whether you want continuous background healing vs. on-demand.
- **Probe tuning / cache.** If many stale sockets accumulate, the ×3s worst
  case grows. Could keep the last-good socket and only re-probe on failure.
- **`SSH_AGENT_DIR` override** already honoured — good for non-default
  layouts.

## 8. Commits

- `9997752` — implement `ssh-heal-forward.sh` + fish function (heal calling
  shell via `export`/`set -gx`).
- `e048af3` — bound the agent probe with a 3s timeout so stale sockets don't
  hang the heal.
