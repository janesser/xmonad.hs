# llama.cpp server — boot autostart: analysis & handover

Status: **fixed and deployed (2026-09-12).** The unit now **fails fast** when the
CUDA backend is unavailable, **self-heals** via `Restart=on-failure`, and orders
after the NVIDIA **kernel driver** so the boot-time CUDA probe no longer races
driver init. One latent issue (the orphan-port race) remains.

This doc evolved across two passes. Keep reading for the correction.

---

## TL;DR — how the diagnosis changed

| | 2026-09-08 → 09-11 (v1) | 2026-09-12 (current) |
|---|---|---|
| OOM root cause | "double launch" (`llama serve` x2) | **CPU fallback when VRAM unusable** → ~26 GB model into system RAM |
| Fix applied | applied then **reverted** | **applied and live** |
| Key insight | two processes, port race | single CPU-backed instance at ~13 GB RAM; CUDA not ready at boot |

---

## 1. The real root cause (corrected)

The OOM was **not** primarily the double-launch. It is:

> When the CUDA backend can't be used, `llama serve` silently falls back to CPU.
> The ~26 GB model then loads into **system RAM** instead of VRAM and trips the
> OOM killer on the 14 GiB box.

Evidence:

- A healthy, GPU-served instance uses only **~1.3 GB** of system RAM:
  ```
  pid 3844 … llama serve …   rss ≈ 1.3 GB ; nvidia-smi 26063 MiB / 32768 MiB used
  ```
- The OOM'd instance sat on **~13 GB** anonymous system RAM:
  ```
  [pid 2166] … llama  total-vm:43220928kB  anon-rss:12914548kB  … oom_score_adj:0
  Out of memory: Killed process 2166 (llama) … anon-rss:12914548kB
  ```
- The kernel trace is a **global OOM** tripped by an unrelated fault, not by
  `llama` asking for memory:
  ```
  fuse_worker invoked oom-killer: gfp_mask=0x140cca(GFP_HIGHUSER_MOVABLE), order=0
  oom-kill:constraint=CONSTRAINT_NONE,…,global_oom,task_memcg=/system.slice/
            restart-llama-server.service,task=llama,pid=2166
  ```
  `fuse_worker` (the passeport **automount** worker) hit a page fault while the
  box was already near full; the OOM killer picked the biggest consumer — the
  CPU-backed `llama` at ~13 GB.

**Conclusion:** the danger is *unusable VRAM → silent CPU fallback → OOM*. That's
exactly what the fail-fast probe (below) now prevents: the unit refuses to start
instead of loading the model onto RAM.

---

## 2. Double-launch — still present, secondary (was the old v1 theory)

`~/.local/bin/restart-llama-server.sh` still runs **two** backgrounded
`llama serve` blocks (the script even carries FIXMEs about it):

* **Block A** — the keeper: binds `:::8080`, actually serves.
* **Block B** — `--models-max 2 --parallel 1 --no-warmup --no-ui --offline
  --models-preset … --verbosity 3 …` with `2>/dev/null >/dev/null`. Never
  intended to serve; a leftover.

If **CUDA is up**, Block B loses the port bind and dies with
`couldn't bind HTTP server socket, port: 8080` — it wastes nothing.
If **CUDA is down**, *both* blocks fall back to CPU → double the ~26 GB into RAM
→ a worse OOM. So Block B is a *latent amplifier* of the root cause in #1, not
its usual direct cause. Neutralize it if you like (echo out Block B, keep A) —
see §5.

Secondary symptom historically seen: `ggml_cuda_init: failed to initialize CUDA`
right before an OOM — a tell that CUDA wasn't ready and it was about to fall
back to CPU (not, as v1 assumed, two processes racing).

---

## 3. Mount precondition — still open (unchanged since v1)

The bind mount that holds the model is a *different* unit from the drive mount
the service orders after.

| Unit | What it is |
|---|---|
| `media-passeport.mount` | external USB drive `/media/passeport` (autofs + btrfs) |
| `home-jan-.cache-huggingface-hub.mount` | **bind mount** `/media/passeport/huggingface-hub/ → ~/.cache/huggingface/hub` — holds the model |

Service orders only:
```
After=local-fs.target media-passeport.mount
```
That ties to the **drive** mount, not the **bind** mount containing the model.
The bind-mount generator unit (`/run/systemd/generator/…`) has no `[Unit]`
ordering — only a `[Mount]` section — so systemd may start it before or after
the service.

Three compounding problems:

1. **It's an automount.** `/media/passeport` is `autofs`; the underlying btrfs
   only materializes when the path is accessed. `After=` is ordering-only, and
   ordering after a *lazy* automount doesn't guarantee storage has
   materialized. Nothing forces the automount to fire before the service runs.
2. **`nofail` silently skips.** `bind,nofail` (fstab line 14) means if the
   passeport isn't ready at boot the bind mount is skipped and boot continues —
   the service then runs `llama serve` against a missing model path → fails.
3. **The script's `sudo mount`/`umount` fallback is dead code.** `mount` and
   `umount` are **not** in jan's passwordless sudoers
   (`/etc/sudoers.d/chezmoi-pi`). So `sudo mount -o bind …` / `sudo umount …`
   get permission-denied. The design therefore rests entirely on the mount being
   pre-done — which isn't guaranteed.

Net: the bind mount works only because something already triggered the automount.
A cold boot where the automount hasn't fired → bind mount can lag/skip → service
runs against a missing model path.

### Where the automount comes from
`media-passeport.automount` is **generated** (ephemeral tmpfs), from `/etc/fstab`
line 13 (`x-systemd.automount`), with line 14 the bind mount:
```
LABEL="passeport"   /media/passeport   btrfs   defaults,nofail,x-systemd.automount 0 2
/media/passeport/huggingface-hub/   /home/jan/.cache/huggingface/hub   none   bind,nofail 0 0
```

---

## 4. Boot CUDA-init race — discovered 2026-09-12 (fixed)

`restart-llama-server.service` orders after `nvidia-persistenced.service`, but
that's **not enough**: `nvidia-persistenced` is a *userspace* daemon that has
**no `After=` on the kernel driver** — it starts before the `nvidia` module
finishes initializing.

Boot timeline (2026-09-12, boot -0) proving the race:
```
20:02:44  kernel: nvidia: loading out-of-tree module …          ← driver starts
20:02:45  Starting nvidia-persistenced.service                  ← daemon up
20:02:45  Starting restart-llama-server.service                 ← our service, concurrent
20:02:46  [drm] Initialized nvidia-drm … on 0000:21:00.0         ← driver FINISHES
20:02:46  no CUDA device found, refusing to fall back to CPU     ← probe FAILS mid-init
```
The probe ran in the ~1 s window while the kernel driver was still initializing.
`Restart=on-failure` (30 s) then recovered it once the driver was ready.

**The fix** (§5) adds ordering after the **kernel driver device unit**
`sys-bus-pci-drivers-nvidia.device` — the real "driver bound" signal — plus a
retried probe so it no longer depends on the 30 s restart to win the race.

---

## 5. What was applied and deployed (2026-09-12)

Unit `etc/systemd/system/restart-llama-server.service` (installed to
`/etc/systemd/system`, `daemon-reload`ed; live, llama serving on :8080):

```ini
[Unit]
After=local-fs.target media-passeport.mount nvidia-persistenced.service
      sys-bus-pci-drivers-nvidia.device
Wants=nvidia-persistenced.service

[Service]
# Fail fast + retry the CUDA probe: refuse to start (let systemd abort) unless a
# CUDA device is available, so we never silently fall back to CPU → OOM.
ExecStartPre=/usr/bin/bash -c 'for i in $(seq 1 8); do
  if llama serve --list-devices 2>/dev/null | grep -q CUDA; then exit 0; fi
  echo "restart-llama-server: CUDA not ready, attempt ${i}/8, retrying…" >&2
  sleep 2
done
echo "restart-llama-server: no CUDA device after 8 attempts, refusing to fall back to CPU" >&2
exit 1'
ExecStart=/usr/bin/fish ~/.local/bin/restart-llama-server.sh
Type=oneshot
RemainAfterExit=yes
Restart=on-failure
RestartSec=30s
```

Script `dot_local/bin/executable_restart-llama-server.sh` (working tree, staged):
- `killall llama-server` / `killall llama` → `if killall llama-server || killall llama; sleep 10; end`
  (settle freed sockets/processes before starting — mitigates the orphan-port race).
- Block A now redirects its stdout/stderr (`>/dev/null 2>/dev/null`).

---

## 6. Remaining open items (decide later)

1. **Orphan-port race.** A `Type=oneshot` that `disown`s a long-lived child leaves
   `llama` in the unit's control group; systemd's cleanup is racy. Boot -1
   showed a textbook failure: an orphaned `llama` held `:8080` and the retry's
   `llama serve` died with `couldn't bind HTTP server socket, port: 8080`. The
   `killall …; sleep 10` tweak mitigates it; a durable fix is `KillMode=cgroup`
   + a clean pre-start teardown, or `Type=simple`.
2. **Mount precondition (§3).** Not addressed. If desired: add the bind mount to
   the service ordering (`Wants=`+`After=home-jan-.cache-huggingface-hub.mount`,
   `Wants=` not `Requires=`), and make the script's mount check non-fatal /
   clear rather than proceeding to a dead model path. Do **not** add `sudo
   mount`/`umount` (not in sudoers — needs explicit approval).
3. **Double-launch (§2).** Optional hygiene: echo out Block B (keeper is Block A).

---

## 7. How to verify
```
journalctl --system -u restart-llama-server.service -f
ss -ltnp | grep 8080
nvidia-smi
```
Expect: a single `llama` serving, `:8080` bound, ~1.3 GB system RAM, **26 GB
VRAM used**, no `oom-kill`, no `couldn't bind`, no CPU-fallback. On a cold boot
the unit should come up cleanly on the first probe attempt (driver ordered via
`sys-bus-pci-drivers-nvidia.device`).

## 8. Files touched
* **Deployed unit:** `etc/systemd/system/restart-llama-server.service`
  (→ `/etc/systemd/system/` via `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`).
* **Script:** `dot_local/bin/executable_restart-llama-server.sh` (working tree).
* **Provisioning:** `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`
  (`UNIT_SRC` retargeted to `etc/systemd/system` after the move).
* **Inputs moved:** `systemd/system/*` → `etc/systemd/system/` (chezmoi layout);
  `.chezmoiscripts/run_onchange_9_1_auto_poweroff_timer.sh.tmpl` retargeted too.
* **Docs:** `README.md`, `AGENTS.md` source-path refs → `etc/systemd/system`.

## 9. Repo hygiene notes (not done — flagging)
* A **dangling symlink** `devices/…/agent_work/Display_adapters` breaks
  `chezmoi` templating / `cz apply` (via `include()`). The unit is installed via
  `sudo install`, so it works regardless — but `cz apply` won't render run
  scripts until that's removed.
* `.chezmoiignore` lists `etc`, `usr`, `discoveries`, `devices`, … — these are
  chezmoi-ignored but git-tracked; the run scripts deploy from the source tree,
  which is the established pattern.
