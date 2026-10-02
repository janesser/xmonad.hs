---
title: Local Multi-Accelerator AI Server (text + image)
status: draft
created: 2026-09-25
updated: 2026-10-02
---

# Local Multi-Accelerator AI Server — text + image

> **Replaces / obsoletes** `discoveries/ai-llama-autostart.md`. That doc's scope
> was "make llama.cpp's single server boot reliably." This is the pivot: a
> hardware-agnostic, multi-model, OpenAI-compatible local AI server spanning
> **text/completion and image generation**.
>
> > **Runtime model decided (2026-09-26).** This doc was authored around
> > **Olla** as the load balancer. §11 resolves that: **LocalAI** is the
> > OpenAI-compatible front *and* the model-lifecycle orchestrator (image via
> > the llama.cpp basis, DG1 dormant). The Olla-vs-gateway question is closed;
> > the remaining items in §11 are rollout verifications, not open design.

## 1. Purpose (one line)

A single-box local AI server that front-end exports **every accelerator the box
has** through an **OpenAI-API-compatible front**, runs **several models
concurrently**, and serves **both text/completion and image generation** —
sized for a small concurrent model portfolio. The nature of that front (a
statically-configured load balancer vs a unified llama.cpp-derived server +
dynamic gateway) is the open question driving this revision; see §11.

## 2. Why this exists

- The previous approach made `llama serve` boot reliably, but its failure mode
  (silent **CPU fallback → OOM on a VRAM-full box**) was the whole problem
  (see `ai-llama-autostart.md`).
- The real need isn't "one model, one backend." The box runs two very different
  models at once on **mixed hardware** (NVIDIA V100 + Intel DG1/IrisXe), so the
  stack must be built around **concurrency and per-accelerator placement**, not
  one well-tuned service.
- **Front + engine = LocalAI** (`mudler/LocalAI`), the OpenAI-compatible front
  *and* the model-lifecycle orchestrator — the question in the original draft
  (Olla-vs-gateway) is resolved in §11. **Not** Ollama the chat app, **not**
  Olla the load balancer, **not** llama.cpp directly.
- **Image generation** rides the **llama.cpp basis** via LocalAI's
  **`stablediffusion-ggml`** (C++/ggml, leejet `stable-diffusion.cpp`) backend,
  so image + text share the V100's single LocalAI instance — see §11. The model
  is consumed as **GGUF** (leejet `Qwen-Image-2.1-GGUF`), **not** the diffusers/
  PyTorch path in `qwen-image-21.md`. INT8 on the V100 (no FP8); see §11a.

## 3. Architecture (the deployed topology)

> **Historical (§3).** This is the prior **Olla** topology. The current runtime
> model is **§11 (LocalAI)**; §3 is kept as historical until the LocalAI rollout.

```
                OpenAI-API clients (pi-agent + others)
                            │  [::]:40114  (public, dual-stack)
                            ▼
                    ┌───────────────┐
                    │      Olla      │  static endpoints + priority,
                    │  load balancer │  routes by exact model name
                    └───────┬───────┘
        ┌───────────────────┼───────────────────┐
        ▼                   ▼                   ▼
  llama-server :8081   llama-server :8082   llama-server :<ROCm>
  NVIDIA CUDA (V100)   Intel SYCL (DG1)     AMD ROCm  ← add a row to scale
        │                   │                   │
   ornith-… (heavy)   LFM2.5-2.6B (Q4_K_M)   (future vendor)
        └────────── huggingface-hub bind-mount (~/.cache/huggingface/hub) ──────────┘
```

- **Olla** = public entry (`[::]:40114`, dual-stack IPv4+IPv6). Config:
  `~/.config/olla/config.yaml`. Routes by **static endpoints + priority**,
  matching each request to the endpoint whose **exact model id** (the
  `--model` path, symlinked-shortened) it reports.
- **One `llama-server` per accelerator** — each backend is an **independent**
  llama.cpp server, isolated per GPU. Today: `:8081` CUDA/NVIDIA,
  `:8082` SYCL/Intel. **AMD = another `llama-server` (ROCm) + one static
  endpoint.**
- **Per-backend boot units** (`etc/systemd/system/`): each is `Type=oneshot`
  whose launcher **detaches** `llama-server` (fork + disown); systemd tracks
  only the launcher, the orphan keeps serving. `KillMode=process`;
  `Restart=on-failure`.
- **Fail-fast, no CPU fallback:** the CUDA probe **refuses to start unless a
  CUDA device is available** — so an unusable-VRAM box aborts rather than
  OOMing. `media-passeport` automount + huggingface-hub bind-mount in `/etc/fstab`
  means no `sudo mount` at boot.

## 4. Requirements

### Functional
- **LocalAI as OpenAI-compatible front + orchestrator.** Single OpenAI-compatible
  entry point that is also the model-lifecycle engine (on-demand load, LRU
  eviction, concurrency-group placement). See §11.
- **One LocalAI instance per accelerator; hardware-agnostic.** Any NVIDIA, Intel,
  or AMD accelerator can serve via LocalAI's per-vendor backend
  (`LOCALAI_FORCE_META_BACKEND_CAPABILITY`); scaling a vendor = new backend, no
  redesign. One instance per accelerator — see §11b (DG1 dormant for now).
- **Concurrent models.** Multiple models of differing size run at once
  (present: a heavy `ornith-*` on CUDA + `LFM2.5-2.6B` on SYCL). Not one
  mega-model.
- **Explicit model selection.** Clients **name the model** (OpenAI `/v1/*`
  style). No default-model / smart-routing layer.
- **OpenAI-API-compatible front.** Any OpenAI-API client (pi-agent and others)
  works out of the box.
- **Reroute or reject — never CPU fallback.** If the backend that serves a
  requested model is unavailable, the request is **rejected** (or rerouted only
  to another backend that *actually serves that model*). It is **never**
  silently served from CPU/RAM.

### Non-functional
- **Fail-fast, visible.** A down/unusable accelerator surfaces loudly, not via
  silent degradation.
- **Portable.** Same setup script runs across boxes with different hardware.

## 5. Scope

| In scope | Out of scope (for now) |
|---|---|
| Text/completion via LocalAI (V100, single instance) | **DG1 / second-accelerator hot path** — dormant until Option 2 (§11b) |
| Image on the llama.cpp basis via LocalAI (Qwen Image, §11a) | **Cross-accelerator reroute / warm failover** — single instance = reject on outage |
| Lifecycle: on-demand load, LRU eviction, no preemption | CPU fallback / degraded serving |
| Default-model / smart routing | Default-model routing — explicit-only for now |
| OpenAI-API-compatible clients | Vendor-specific clients / protocols |
| Concurrent small/mid model portfolio | One mega-model; GPU sharding |

## 6. Clients

All **OpenAI-API-compatible** clients. pi-agent is one example, not the design
target — the interface is defined for *any* such client. (Open question: which
concrete clients — a chat UI like OpenWebUI/NextChat, scripts, …? Affects only
what we test, not the API surface.)

## 7. Workload envelope

- **Present:** 2–3 small-to-mid LLMs running concurrently, ~2.6B to ~35B
  parameters, quantized across a 32 GB V100 + integrated graphics.
- **Similar (horizon):** a few more models of comparable size; a bigger single
  model that may need sharding across GPUs. *Image gen sits beside this envelope
  (§11), not inside the LLM size range.*

## 8. Key open questions (load-bearing)

- **[DECIDED] Front + engine = LocalAI** (§11). Resolves the original
  Olla-vs-gateway question: LocalAI is the OpenAI-compatible front *and* the
  lifecycle orchestrator. With one V100 instance the router largely dissolves;
  a router returns only if DG1 becomes hot (Option 2).
- **[DECIDED] DG1 dormant by default** (§11b). The DG1/small-model role is
  secondary and unused; one LocalAI instance on the V100 hosts text + image.
  DG1 goes hot only if a real DG1 workload appears (Option 2, data-driven
  router).
- **[QUESTION] Reject on outage is the failover policy.** With a single V100
  instance and DG1 dormant, a down LocalAI = **reject** (no warm failover).
  Confirm reject — rather than a standby backend — is acceptable.
- **[QUESTION] AMD ROCm — horizon marker.** "Any NVIDIA/Intel/AMD" names ROCm,
  but only CUDA is deployed. Near-term target or horizon?
- **[VERIFY] Exact model ids.** Confirm the canonical reported model names
  LocalAI exposes (config said `ornith-35B`; elsewhere `ornith-1.5-27B`).
- **[OPEN] #1498 backend switch.** If image + text ever share one GPU instance,
  validate the text<->image backend switch; keep one active backend at a time
  (SINGLE_ACTIVE_BACKEND) until fixed.
- **[OPEN] Intel SYCL on DG1.** Verify driver/SDK prerequisites before DG1 is
  anything more than dormant; skip until Option 2.
  that may dissolve this question.

## 9. Deployment

**Incidental, not product.** The deliverable is the server design + setup
script (the backend-table provisioning, e.g.
`.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`). Installation is
whatever is convenient — chezmoi run script, plain bootstrap, systemd unit —
and must be **portable across hardware**. This brief does **not** constrain the
installer.

## 10. Success criteria

- Multiple models of different sizes run concurrently, each on an accelerator
  that can serve it.
- An OpenAI-API client can request any model by name and get a response via
  LocalAI.
- A model loads on first request and evicts idle (LRU); no model is preempted
  mid-run — a long-running gen blocks shorter requests until its load completes
  (FIFO, no preemption).
- When the V100 instance is down, the request **rejects** (no warm failover while
  DG1 is dormant) — never silently falls back to CPU.
- Adding a new-vendor backend (e.g. AMD ROCm) is a backend-table row + one
  static endpoint, not a redesign.
- The same setup script runs on a box with different hardware and adapts.

## 11. Runtime model — LocalAI as orchestrator + OpenAI front

**Decision (2026-09-26): LocalAI (`mudler/LocalAI`) is the front AND the
lifecycle engine.** The "custom per-model orchestrator" §11b's earlier draft
doesn't need building — LocalAI ships it, with image included. This collapses
the old router-vs-Olla question: with one instance the router largely
dissolves.

### 11a. LocalAI is the lifecycle engine we designed

Every piece we were going to hand-write is first-party LocalAI:

| Lifecycle need | LocalAI mechanism |
|---|---|
| Load a model on first request | **ModelLoader** — on-demand load + automatic backend selection |
| Evict idle models | **`--max-active-backends`** LRU eviction |
| Unload a stuck / busy model | **watchdog busy-timeout** |
| Drop idle models after a while | **watchdog idle-timeout** (default 15m) |
| Placement / mutual exclusion | **concurrency groups** — per-model anti-affinity, "load one evicts the others" |
| Text + image + audio | **llama-cpp** (incl. **Intel SYCL**), **stablediffusion-ggml** (C++/ggml), **whisper** under one OpenAI-compatible front |
| Per-model backend + GPU | per-model YAML `backend:` + `LOCALAI_FORCE_META_BACKEND_CAPABILITY=nvidia|amd|intel` |

This gives the exact behavior we specified: on-demand load, LRU eviction,
FIFO/no-preemption (an actively-served model is never evicted — only the
least-recently-used idle one), per-model placement. **Request-significance
timeouts** ("a request might lose significance") are a **client-side HTTP
timeout**, not a LocalAI concern; LocalAI's watchdog times *model* residency,
not queue-wait. So no custom timeout logic.

Image rides the llama.cpp basis (leejet `stable-diffusion.cpp`, via LocalAI's
**`stablediffusion-ggml`** C++/ggml backend) and keeps the single-stack win:
Qwen Image 2.1 is available as **GGUF** (`leejet/Qwen-Image-2.1-GGUF`), speaks
OpenAI `/v1/images/generations`, and shares the V100's CUDA backend with text. V100
INT8 reality unchanged — hardware INT8 + FP16, no FP8; INT8 is the right quant,
and 32 GB clears the ≥16 GB gate.

> **Backend choice matters.** LocalAI *also* ships a Python **`diffusers`** image
> backend — a separate torch process with no single-stack win and its own
> dependency surface. The single-stack claim here requires the
> **`stablediffusion-ggml`** backend specifically. It is also the LocalAI backend
> that carries **Intel SYCL**, so Option 2 (§11b) keeps a SYCL image path too.

### 11b. Topology — one instance per accelerator; DG1 dormant by choice

LocalAI runs as one process and binds to **one vendor/accelerator**, so it does
not split the V100 and DG1 in a single instance (and a current bug, #1498, makes
text<->image backend-switching in one GPU instance fragile). Two options:

- **Option 1 — one LocalAI instance on the V100, DG1 dormant.** The V100 hosts
  big-text + Qwen-Image + small-text. No cross-vendor router, no #1498 exposure.
  **Chosen by default:** the DG1/small-model role is secondary and unused, so
  keeping a second instance alive "just in case" is dead weight.
- **Option 2 — two instances (V100/CUDA + DG1/SYCL) behind a data-driven router
  (LiteLLM/Olla).** Only justified once an actual DG1 workload appears. Re-
  introduces a router, but a data-driven one, not the hand-edited Olla config.

With Option 1 the **router question dissolves** — clients hit LocalAI directly;
the router is only brought in if Option 2 grows.

### 11c. Open verification items (gate the rollout)

- **#1498 — text<->image backend switch in one GPU instance.** If anything ever
  runs image + text in one instance, test it now; `SINGLE_ACTIVE_BACKEND` (one
  active backend at a time) matches the no-co-reside lifecycle and is the safe
  posture regardless.
- **Intel SYCL on the DG1.** Driver/runtime prerequisites before DG1 is anything
  more than dormant; skip until Option 2 is warranted.
- **`pinned`-model bug (#11101):** on some versions a `pinned: true` model still
  tears down per-request, paying a cold reload — test residency policy on the
  shipped version.
- **Version pin:** like the prior Olla pin, LocalAI's behavior rides its version;
  pin it once the topology is validated.

> **Supersedes §3's Olla diagram.** The deployed topology in §3 is the prior
> Olla design; §11 is the current runtime model. §3 stays as historical until
> the LocalAI rollout is deployed.

## 12. Risks

- **Single V100 instance is a public chokepoint.** One LocalAI process serves
  everything; if it dies or wedges the whole front is down and DG1 is dormant,
  so there is no warm failover. Failover is model-dependent and this box has no
  standby — reject-on-outage is the policy (§8). A second instance (Option 2)
  would add a failover target but not a failover *path* without a router.
- **#1498 backend-switch fragility.** Mixing image (diffusers) and text
  (llama-cpp) backends in one GPU instance currently breaks on a backend switch;
  keep one active backend at a time (`SINGLE_ACTIVE_BACKEND`) until fixed.
- **Concurrency capacity.** Big-text + Qwen-Image INT8 are effectively
  mutually exclusive on the 32 GB V100; concurrency groups + LRU eviction encode
  the "load one evicts the others" policy but must be tuned to the actual model
  sizes.
- **Residency policy is version-sensitive.** `pinned`/idle-timeout eviction
  behaves differently across LocalAI versions (#11101); validate the shipped
  version's eviction before relying on a model staying resident.
- **Client-side request timeouts.** "A request loses significance" is enforced by
  the client, not LocalAI — make sure the OpenAI clients carry sane timeouts, or
  a long cold-load queue can hang a client.

---

## Handover — two in-progress threads (2026-10-02)

Two separate jobs were worked on recently. Both are mid-flight; here's the state
and the exact next steps for whichever assistant picks them up.

### A. HuggingFace cache migration: `/media/passeport` sda1 → sdb1 (M.2)

**Context.** The HF hub bind-mount (`/media/passeport` → `~/.cache/huggingface/hub`,
the llama backend's model cache) lived on `sda1` (USB "My Passport", 466 G,
88% full). Moving it to an internal M.2 (`sdb1`, WD Blue SA510 1000 G).

**Status — as of this writing (live-checked today):**

- ✅ `sdb1` wiped (it held a Kali Live ISO — safe junk) + reformatted btrfs with
  **`LABEL="passeport"`** so fstab's `x-systemd.automount` keeps matching it.
- ✅ Baseline + target benchmarks captured.
- ⏳ **Mirror still running:** `sudo rsync -aHAXx --delete /media/passeport/ /mnt/new/`
  (root, started ~15:59 today, still going — ~400 G tail on sdb1's ~260 MB/s
  QLC-limited writes).
- ⏳ **Switch + rebind NOT done.** Still serving from `sda1`.
- ⏳ **Temp sudoers drop-in `zz_sdb_migrate` still installed** (only
  `parted/mkfs.btrfs/wipefs/mount/umount/rsync`; remove when done).

**Key facts for whoever finishes it:**

- `sdb1` write is **~2× slower** than sda1's read side (260 MB/s sustained vs
  ~932 MB/s) — QLC cache exhaustion, on a proper SATA III link (not a port
  problem). Fine for a read/model-serving model cache; don't re-benchmark as if
  it were a perf regression.
- `sdb1` wedged once during format (no logged error, died on its own) — clear a
  reboot if it ever hangs again; run `smartctl` self-check if it wedges a
  second time before trusting it with 400 G. (`smartctl` needs root — it's not
  in the temp drop-in.)

**Remaining sequence (all the rest after the mirror finishes):**

1. **Verify** `/mnt/new` == `/media/passeport` (`diff -r` or checksum/`du`).
2. **Switch + rebind:** unmount HF bind → `umount /media/passeport` (sda1) →
   `mount` sdb1 by LABEL `passeport` at `/media/passeport` → remount the HF
   bind → restart the llama backend. (fstab needs no edit — it keys off the
   label; the bind line is unchanged.)
3. **Clean up:** remove the `zz_sdb_migrate` drop-in, unmount `/mnt/new`.

### B. GPU dual-driver: nouveau + nvidia_drm (GT 730)

**Context.** Wanted the GT 730 (GK208B, PCI `17:00.0`) off `simple-framebuffer`
and onto real accel, coexisting with the V100 on `nvidia`.

**Status — as of this writing (live-checked today):**

- ✅ **V100** (`21:00.0`) still bound to **nvidia 580** (`nvidia_drm`+modeset) — fine.
- ✅ **Iris Xe DG1** (`2f:00.0`) bound to **i915** — fine.
- ✅ **GT 730** (`17:00.0`) is **now bound to `nouveau`** (live `lsmod` +
  `/sys/.../17:00.0/driver → nouveau`). The `modprobe nouveau` test from the
  earlier session **succeeded** — nouveau initializes the GK208B on this box.
- ⏳ **Durability NOT done.** No udev driver-pinning rule yet — this resets on
  reboot.

**Remaining step (one, durable):**

1. Write the per-device udev pin so it's order-independent and nouveau can
   never grab the V100 at boot (there's still no nvidia initramfs hook to save
   us today). Draft already prepared in-session as
   `/etc/udev/rules.d/60-gpu-driver.rules`:
   - `10de:1db5` (V100) → `driver_override nvidia`
   - `10de:1287` (GT 730) → `driver_override nouveau`
   then `udevadm control --reload; udevadm trigger` (or reboot) and confirm
   `journalctl -k | grep -E 'nvidia|nouveau'` shows both bound correctly on
   next boot.
- `modprobe`/`udevadm trigger` need root and are **not** in any current
  allowlist — either extend the temp drop-in (rename it) or run this step
  interactively with your password.

### C. LocalAI deployment — deployed & running, but the rollout is unfinished

**Context.** This is the LocalAI thread (§11). It was **deployed and is live**
since 2026-10-02 — unlike A and B, this one works, it is just not *finished*.
Live-checked 2026-10-02:

- ✅ **Running + functional.** `localai run --address=[::]:8080` (v4.10.0,
  PID 87951). The Intel **SYCL** backend is real: a timed completion against
  `qwen-sycl` returned the exact expected string — it is serving on the Iris Xe
  GPU, not silently on CPU.
- ✅ **Models present.** `qwen-sycl` (Intel SYCL, llama-cpp backend),
  `antares-1b` + `qwen-05b` (CPU). Data dirs under `~/.local/share/localai/`.
- ✅ **chezmoi-managed.** Deployed by the tracked
  `run_once_5_aitools_5localai_startup.sh` (pinned `v4.10.0`, downloads the
  precompiled binary — no docker). Installs the host Intel Level Zero driver
  (`libze-intel-gpu1`, the bundled SYCL driver predates the kernel 7.0 i915
  ABI).

Despite that, the **staged cutover is only half done**. Open items, in order of
severity. **pi-agent routing verified 2026-10-02** (details under item 4): the
`olla` autodetect provider points at `OLLA_BASE_URL=http://127.0.0.1:8080`, and a
`pi --print --provider olla --model qwen-sycl` completion returns cleanly through
the Iris Xe SYCL backend.

That said, the **staged cutover is only half done**. Open items, in order of
severity:

1. **✅ Boot-persistent (verified 2026-10-03; `Linger=yes`).** The live unit is a
   **`--user` service** (enabled, `WantedBy=default.target`) — correct, because
   SYCL fails under a `--system` unit on this box (needs jan's full group set,
   incl. `render`/`video`, and there is no device cgroup under the user
   manager). `loginctl show-user jan | Linger` is now **`yes`** (the
   `sudo loginctl enable-linger jan` step from the prior session is done), so
   `user@1000.service` starts at boot before login and this unit comes up with
   it. `user@1000.service` shows `loaded active running`. **Remaining proof:**
   a literal reboot test (only unconfirmed step) — do not reboot the live box
   unilaterally; confirm after the next scheduled restart that `localai`
   answers on :8080 before any pre-login session.
2. **The staged cutover to the V100/CUDA end-state was never reached.** The run
   script's whole intent (and the `localai.service` comments) is a **two-phase**
   cutover: LocalAI pinned to **Intel SYCL now** (so a model serves with **no
   V100 VRAM contention** while Olla + llama-cuda hold the V100), then at the
   `llama-cuda` cutover the unit's `LOCALAI_FORCE_META_BACKEND_CAPABILITY` is
   flipped to **`nvidia`** for the end-state. That flip **never happened** —
   LocalAI is still pinned to `intel`, and there is **no** Olla/llama-cuda
   process running now. **Decision needed:** either (a) proceed with the flip to
   the V100/CUDA end-state (the documented end-state, §11 Option 1), or (b)
   formally abandon the V100 cutover and accept Intel SYCL as the running
   config — and update this doc accordingly. Don't leave it parked mid-cutover.
3. **Stale redundant `--system` unit — RETIRED 2026-10-03.** `~/.config/systemd/user/localai.service`
   is the live one; the `/etc/systemd/system/localai.service` fallback (was
   disabled/inactive) existed only *"until the --user unit serves a SYCL model
   on :8080"* — that condition is met, so the --user unit is now the sole path.
   **Done:** `sudo rm /etc/systemd/system/localai.service` + `sudo systemctl
   daemon-reload` (both NOPASSWD under the chezmoi-pi drop-in). Verified:
   `is-enabled` = `not-found`, no `localai` unit files remain in any systemd
   search path, and the live `--user` unit stays `active`.
   **Repo cleaned to match:** the `run_once_5_aitools_5localai_startup.sh`
   `--system` deploy block, its now-unused `UNIT_*` vars, and the
   `etc/systemd/system/localai.service` source were removed (and the run
   script's stale comments updated). The run script was already a one-shot, so
   this has no live effect — it just stops the unit from ever being re-added
   and removes the confusion hazard.
4. **`context_size` is tiny → full pi-agent runs rejected (live-confirmed).** `qwen-sycl.gguf.yaml`
   sets `context_size: 2048` (effective runtime tuning observed at 8192);
   any request over the cap is rejected with *"request exceeds the available
   context size."*

   **pi-agent test, 2026-10-02:** a full agent run (tools **on**) = **15229
   tokens** → `rpc error: Internal … exceeds the available context size
   (8192 tokens)`. The same prompt with `--no-tools` fits under 8192 and returns
   `PI_SYCL_OK` end-to-end. So the SYCL path *works* in pi-agent; the 8 KB window
   is simply too small for a tool-carrying agent prompt (system prompt + loaded
   AGENTS.md files + tool schemas). This is a config knob, not an architecture
   problem.
   **RESOLVED 2026-10-03 — raise `context_size`, but it must be a TOP-LEVEL YAML
   key, not under `parameters:`.** LocalAI's llama-cpp schema reads
   `context_size` as a model-level field (like `name`/`backend`); under
   `parameters:` it maps to llama.cpp `--params` and is ignored, so LocalAI
   fell back to its hardcoded default of 8192 (`Estimate used default
   context_size=8192`). Set `context_size: 32768` at the top level of
   `qwen-sycl.gguf.yaml`; restart the user unit. Verified: effective tuning now
   reports `context=32768 … n_gpu_layers=99999999 parallel=8 f16=true` — SYCL
   GPU placement intact, parallelism restored. Full pi-agent run (tools **on**,
   ~15229 tokens) now completes end-to-end → `FULL_AGENT_OK` (was rejected at
   8192). No `LOCALAI_DISABLE_HARDWARE_DEFAULTS` needed.
   **Speed caveat (important):** qwen-0.5b on the Iris Xe iGPU is slow — prompt
   processing ≈ 60 tok/s, so a full agent round takes ~5-6 min (a 15k-token
   first round alone is ~200s). It *works*, but don't expect snappy tool use;
   it's a small-model iGPU, not a V100. 32768 = the model's trained max context;
   KV cache sits ~750 MB RSS, fine on shared VRAM.
5. **Image-gen path (Qwen Image / `stablediffusion-ggml`) never deployed.**
   §11a's image-on-the-llama.cpp-basis (GGUF Qwen Image, INT8 on the V100)
   is still only in the design. **Open:** implement once the text backend
   placement (item 2) is settled and `SINGLE_ACTIVE_BACKEND` is confirmed.
6. **model-discovery integration: null, fall back to `models.json`.** The last
   session's finding stands: LocalAI's `/v1/models` returns only `{id, object}`
   with **no** metadata, so `context_length`/`max_completion_tokens` are all
   `None` and model-discovery has nothing to auto-detect — its value is
   essentially null for LocalAI. **Decision recorded but not implemented:** use a
   hand-edited `models.json` instead. Whoever takes it up confirms the file
   location/schema pi expects.
7. **Security posture — live, no auth.** Bound to `[::]:8080` with
   `--allow-insecure-public-bind` (no API key / no `--auth`), mirroring the old
   Olla posture. Works for a trusted LAN but means **any** host on the network
   can hit the AI front unauthenticated. Keep or add auth — this is a deliberate
   open call, not an oversight.
8. **Version pin (minor).** §11c said pin LocalAI's version once the topology
   is validated. It is pinned at `v4.10.0` in the run script; confirm that
   pin is the validated one and stop chasing upgrades until the topology
   (item 2) is finally decided.
