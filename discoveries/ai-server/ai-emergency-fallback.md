---
title: llama.cpp Emergency Fallback + Build Safespot
status: draft
created: 2026-10-05
updated: 2026-10-05
---

# llama.cpp Emergency Fallback + Build Safespot

> **Sibling of** `discoveries/ai-server.md` (target topology / vLLM-Omni) and
> `discoveries/llama-cpp-optimizations/` (tuning). This doc covers the **stopgap
> serving path** and the **archive of the pinned production build** — neither of
> which the target-topology doc discusses.

## 1. Purpose (one line)

Provide a fast, manual **stopgap backend** when the managed router/Olla path is
down, **and** preserve the exact pinned `llama.cpp` build as a rollable-back
snapshot so the v0.5.0 weakness can be fixed without losing the known-good
binaries.

## 2. Context — the emergency path and the v0.5.0 weakness

- **Emergency launcher:** `~/.local/bin/llama-emergency.sh` (chezmoi:
  `dot_local/bin/executable_llama-emergency.sh`). It stops the managed units
  that can hold the V100 (`llama-cuda`, `llama-sycl`, `vllm-omni`), reaps any
  server on the emergency port, and serves ONE bare model with the required
  `--parallel 1 --device CUDA0`. See its header `# Usage` / `# Notes` for the
  full flag surface.
- **Pinned build:** the `llama` / `llama-server` symlinks point at
  `~/projs/llama.cpp/build_cuda/bin/`, a **cleanly-built `0.5.0` at commit
  `7fe450e19`** (`llama --version` → `0.5.0-dev (build 11146, commit 7fe450e19)`;
  git tag/PR #29333 "bump version to 0.5.0").
- **The weakness:** `0.5.0` carries a **known vulnerability**. It is a
  stopgap version, not the target runtime, so the plan is to **fix/upgrade the
  build** — while keeping `7fe450e19` available as a rollback point. The source
  of that exact build is preserved in the fork `janesser/llama.cpp.git`
  (`git show 7fe450e19` resolves), so it is reproducible; the snapshot below is
  the *byte-exact* copy that avoids a rebuild.

## 3. The safespot — `~/.local/share/llama-emergency`

An archive of the **current production binaries**. This directory is
**runtime DATA, not a chezmoi dotfile** — it lives outside chezmoi and git;
only the helper script is version-controlled (durability). Layout:

```
~/.local/share/llama-emergency/
├── snapshots/
│   ├── index.jsonl                       # one JSON line per snapshot (registry)
│   └── llama-cuda-7fe450e19-<stamp>.tar.gz
├── meta/
│   └── llama-cuda-7fe450e19.meta         # ref, commit, version, sha256, path
└── log/
    └── snapshot.log
```

## 4. Helper script — `~/.local/bin/llama-emergency-snapshot.sh`

Chezmoi source: `dot_local/bin/executable_llama-emergency-snapshot.sh`
(rendered into `~/.local/bin` by `cz apply`; it never sudo-mounts and never
touches the live build during a snapshot — the tar is a copy).

```
llama-emergency-snapshot.sh snapshot     # archive current prod build (idempotent)
llama-emergency-snapshot.sh list         # print the jsonl index
llama-emergency-snapshot.sh restore <ref> [--dest DIR] [--yes]   # roll back
```

- **Host resolution:** the emergency server binds the **current LAN address**,
    auto-resolved via `ip route get` (a pure route lookup — no packets sent),
    so a new DHCP lease keeps it reachable without editing the script.
    Override with `LLAMA_HOST` (e.g. `LLAMA_HOST=127.0.0.1` for local-only).
    Falls back to `127.0.0.1` + a warning if nothing routable is found.
- **Idempotent:** a `ref` + `backend` already snapshotted is skipped, not
  duplicated.
- **Safe restore:** restoring over the LIVE prod bin requires an explicit
  `--yes`; a bad ref errors and points at `list`.
- **Env knobs:** `LLAMA_EMERGENCY_ROOT`, `LLAMA_PROD_SRC`, `LLAMA_PROD_BIN`,
  `LLAMA_BACKEND` (all overridable; defaults target this box).

## 5. What was archived (2026-10-05)

| Field | Value |
|---|---|
| ref / commit | `7fe450e19` / `7fe450e19305b828c199d602c23a8337aaa1f03b` |
| version | `0.5.0-dev` (CUDA build, x86_64, host `cyberkleiber`) |
| prod bin | `~/projs/llama.cpp/build_cuda/bin` (423 MB tree) |
| tar | `snapshots/llama-cuda-7fe450e19-20261005T185652.tar.gz` (~105 MB) |
| sha256 | `9bdf0c9abf6b3489acce23e69ca8250b6ea16668042c84db5a7cad5da214d606` |

See `meta/llama-cuda-7fe450e19.meta` for the canonical record.

## 6. Rollback / restore

```bash
# Over the live prod bin (needs --yes):
llama-emergency-snapshot.sh restore 7fe450e19 --yes

# Or to a separate dir (e.g. before rebuilding build_cuda):
llama-emergency-snapshot.sh restore 7fe450e19 --dest ~/projs/llama.cpp/build_cuda.frozen
```

The frozen dir keeps the symlinks intact (the tar stores relative paths), so it
can be re-pointed at directly if the fix regresses.

## 7. Durability (off-disk)

The safespot currently sits on the internal NVMe (`/`, ~9 GB free). For true
off-box durability, copy the tarball to the external drive:

```bash
cp -p ~/.local/share/llama-emergency/snapshots/*.tar.gz /media/passeport/
```

(`/media/passeport` is the sda1→sdb1 bind-mount; verify it is mounted first.)

## 8. Relationship to the rest of the stack

- **`llama-integration-test.sh`** (chezmoi `dot_local/bin/executable_llama-integration-test.sh`):
  **kept as-is.** It builds each `--ref` (default `v0.5.0`) in an *isolated*
  workspace under `~/.local/share/llama-integration-test` and never touches the
  production build — it is the *pre-flight* harness; the safespot is the
  *rollback* archive. The two are complementary, not overlapping.
- **Symlinks** (`dot_local/bin/symlink_llama`, `symlink_llama-server`) point at
  `…/build_cuda/bin/…`; the snapshot/restore operates on that same `bin/`.
- **Systemd units** (`llama-cuda`, …) are managed separately; the emergency
  launcher stops them, the snapshot script does not touch them.

## 9. Open items / decisions

- [ ] Decide fix target: rebuild `build_cuda` from a patched commit, or drop in
      a fixed upstream `v0.5.x`. Until then `7fe450e19` remains the active build.
- [ ] Confirm an off-disk copy to `/media/passeport` was made (see §7).
- [ ] The emergency server still binds with **no auth** (trusted home net).
      If the LAN isn't trusted, either add an auth token upstream or bind
      local-only via `LLAMA_HOST=127.0.0.1`.
