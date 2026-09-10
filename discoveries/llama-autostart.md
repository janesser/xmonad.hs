# llama.cpp server — boot autostart: analysis & handover

Status: **analysis done, changes reverted.** No persistent change is in place.
Everything below is evidence + findings so the next pass can fix it properly.

## 1. What was broken

`restart-llama-server.service` (system unit, `enabled`) keeps dying at boot:

```
Active: failed (Result: oom-kill)
...
systemd[1]: The kernel OOM killer killed some processes in this unit.
systemd[1]: Failed with result 'oom-kill'.   (peak ~12.4 GiB RAM)
```

Repeated OOM kills, one per boot, e.g.:
`Sep 09 10:54`, `Sep 10 07:46`, `Sep 10 20:08`.

Machine: `cyberkleiber`, NVIDIA Tesla V100 32 GB, system RAM **14 GiB** (+4 GiB swap).
Model: `Ornith-1.5-35B-A3B-GGUF` `Q4_K_M` (~26 GB in VRAM), served on `:::8080`.

## 2. Root cause #1 — the launcher starts `llama serve` TWICE

File: `~/.local/bin/restart-llama-server.sh`
(chezmoi source: `dot_local/bin/executable_restart-llama-server.sh`)

It launches two separate backgrounded `llama serve` invocations, both binding
`:::8080`. The script even carries the author's FIXMEs:

```
# FIXME log file is re-used/overwritten by slave process actually loading the model ...
# FIXME router process starts router process (no typo)
```

Both processes load the model into VRAM + set up CUDA contexts, then the loser
dies on bind:

```
E srv  start: couldn't bind HTTP server socket, hostname: ::, port: 8080
E srv  llama_server: exiting due to HTTP server error
```

Loading the ~26 GB model **twice** spikes RAM and trips the OOM killer on the
14 GiB box. The two blocks:

* **Block A** — `--model …/Ornith-1.5-35B-Q4_K_M.gguf --log-file …` — this is the
  one that actually serves (it wins the port). Verified live:
  `llama serve --host :: --model …/Ornith-…-Q4_K_M.gguf --log-file …/llama-server.log`.
* **Block B** — `--models-max 2 --parallel 1 --no-warmup --no-ui --offline
  --models-preset ~/.llama-cpp-models-preset.ini --verbosity 3 --log-file …`
  with output discarded (`2>/dev/null >/dev/null`). Never runs; only wastes RAM.

Secondary symptom at boot: `ggml_cuda_init: failed to initialize CUDA: unknown
error` appears right before each OOM — the two processes racing the CUDA driver;
once the loser exits the survivor inits fine.

Current healthy state (manual): `pid 5176`, 26 GB of the 32 GB V100 in use,
serving on 8080.

### Fix that was applied and then REVERTED
The duplicate was neutralized by turning Block B into an `echo` (prints instead
of executes) — the author's usual convention. It was applied via
`chezmoi apply -- ~/.local/bin/restart-llama-server.sh`, verified with
`fish -n` and an echo-only run test (no process spawned), then **reverted** with
`git checkout -- dot_local/bin/executable_restart-llama-server.sh`. Source tree
is clean. Keep this approach if chosen later: Block A is the keeper (it's the one
that binds/serves), so echo out Block B.

## 3. Root cause #2 — the mount precondition is not reliably assured

The bind mount that holds the model is a *different* unit from the drive mount
the service orders after.

| Unit | What it is |
|------|-----------|
| `media-passeport.mount` | external USB drive `/media/passeport` (autofs + btrfs) |
| `home-jan-.cache-huggingface-hub.mount` | **bind mount** `/media/passeport/huggingface-hub/ → ~/.cache/huggingface/hub` — this holds the model |

Service orders only:
```
After=local-fs.target media-passeport.mount
```
That ties to the **drive** mount, not the **bind** mount that contains the model.
The bind-mount generator unit (`/run/systemd/generator/…`) has no `[Unit]`
ordering — only a `[Mount]` section — so systemd may start it before *or* after
the service. No ordering links the service to the unit that holds the model.

Three compounding problems:

1. **It's an automount.** `/media/passeport` is `autofs`; the underlying btrfs
   only materializes when the path is accessed. `After=` is ordering-only, not a
   hard dependency, and ordering after a *lazy* automount doesn't guarantee the
   storage has materialized. Nothing forces the automount to fire before the
   service runs.
2. **`nofail` silently skips.** On line 14, `bind,nofail` means if the passeport
   isn't ready at boot the bind mount is skipped and boot continues — the service
   then runs `llama serve` against a missing model path → fails regardless.
3. **The script's `sudo mount`/`umount` fallback is dead code.** `mount` and
   `umount` are **not** in jan's passwordless sudoers (`/etc/sudoers.d/chezmoi-pi`)
   — `sudo -l -U jan` shows neither. So `sudo mount -o bind …` / `sudo umount …`
   in the script get permission-denied and can't recover. The design therefore
   rests entirely on the mount being pre-done — which is not guaranteed.

Net: the bind mount works only because something already triggered the
automount. At a cold boot where the automount hasn't fired, the bind mount can
lag or skip and the service starts against a missing model.

### Where the automount comes from
`media-passeport.automount` is **generated**, not a custom unit (ephemeral
tmpfs):

* Generated unit: `/run/systemd/generator/media-passeport.automount`
* Boot-order symlink: `/run/systemd/generator/local-fs.target.wants/media-passeport.automount`
* Source of truth: `/etc/fstab` **line 13**:
  ```
  LABEL="passeport"	/media/passeport	btrfs	defaults,nofail,x-systemd.automount 0 2
  ```
  The `x-systemd.automount` option generates the `[Automount]` unit. Line 14 is
  the bind mount:
  ```
  /media/passeport/huggingface-hub/ /home/jan/.cache/huggingface/hub none bind,nofail 0 0
  ```

## 4. The systemd unit itself (healthy)

`systemd/system/restart-llama-server.service` is structurally fine and does not
need changing for the OOM:

* System unit (not user) → starts pre-login, no `loginctl enable-linger` needed.
* `Type=oneshot`, `RemainAfterExit=yes`, `KillMode=process` → leaves the orphaned
  `llama` running after the oneshot launcher exits.
* `ExecStart=/usr/bin/fish ~/.local/bin/restart-llama-server.sh`.
* `WantedBy=multi-user.target`.

## 5. Recommended next steps (pending author decision)

1. **Kill the double launch.** Keep Block A, neutralize Block B. Preferred method
   (author's convention): turn Block B into an `echo` so it prints the would-be
   command as a record without executing it. Alternative: delete Block B.
2. **Tighten the mount dependency.** In the service unit add the bind mount to
   the ordering, e.g.:
   ```
   After=local-fs.target media-passeport.automount
           home-jan-.cache-huggingface-hub.mount
   Wants=home-jan-.cache-huggingface-hub.mount
   ```
   `Wants=` (not `Requires=`) avoids hard-failing boot if the drive is absent,
   while still starting the bind mount before the service. Editable sources:
   `/etc/fstab` (line 14) + `systemd/system/restart-llama-server.service`.
3. **Make the script's mount check non-fatal / clear.** If the hub isn't already
   mounted, log/`notify-send` "passeport not ready" and `exit 1` rather than
   proceeding to a dead model path. Do not rely on `sudo mount`/`umount` (not in
   sudoers) — that would touch the sudo boundary and needs explicit approval.

## 6. How to verify after a fix
```
journalctl --system -u restart-llama-server.service -f
ss -ltnp | grep 8080
nvidia-smi
```
Expect: a single `listening on http://[::]:8080`, one `llama` process, no
`oom-kill`, no `couldn't bind` / `ggml_cuda_init` errors.

## 7. Files touched
* Read/analyzed: `/etc/systemd/system/restart-llama-server.service`,
  `/etc/fstab`, `/run/systemd/generator/media-passeport.automount`,
  `~/.local/bin/restart-llama-server.sh`.
* Provisioning script: `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`
  (installs the unit + fstab bind mount; idempotent, change-gated).
* Reverted: `dot_local/bin/executable_restart-llama-server.sh` (back to original).
