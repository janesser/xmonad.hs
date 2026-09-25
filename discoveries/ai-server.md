---
title: Local Multi-Accelerator AI Server (Olla front)
status: draft
created: 2026-09-25
updated: 2026-09-25
---

# Local Multi-Accelerator AI Server — Olla-fronted

> **Replaces / obsoletes** `discoveries/ai-llama-autostart.md`. That doc's scope
> was "make llama.cpp's single server boot reliably." This is the pivot: a
> hardware-agnostic, multi-model, **Olla-fronted** local AI server.

## 1. Purpose (one line)

A single-box local AI server that front-end exports **every accelerator the box
has** through **Olla acting as the load balancer**, runs **several models
concurrently**, and serves an **OpenAI-API-compatible** front so any OpenAI
client works — sized for a small concurrent model portfolio, with image
generation parked for later.

## 2. Why this exists

- The previous approach made `llama serve` boot reliably, but its failure mode
  (silent **CPU fallback → OOM on a VRAM-full box**) was the whole problem
  (see `ai-llama-autostart.md`).
- The real need isn't "one model, one backend." The box runs two very different
  models at once on **mixed hardware** (NVIDIA V100 + Intel DG1/IrisXe), so the
  stack must be built around **concurrency and per-accelerator placement**, not
  one well-tuned service.
- **Olla** (the `thushan/olla` OpenAI-compatible load balancer) is chosen as
  the front — **not** Ollama the chat app, and **not** llama.cpp directly.

## 3. Architecture (the deployed topology)

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
- **Olla as load balancer / front.** Single OpenAI-compatible entry point that
  routes across the box's accelerators.
- **One `llama-server` per accelerator; hardware-agnostic.** Any NVIDIA, Intel,
  or AMD accelerator can serve — scaling a new vendor = add a `llama-server`
  backend + one static endpoint (no redesign). The provisioning script already
  drives a **backend table** (`UNIT|SCRIPT|PORT|START`), so new backends are a
  row, not a rewrite.
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
| Text/completion inference via Olla | **Image generation** (deferred — see §8) |
| Multiple concurrent models, one `llama-server` per accelerator | Multi-machine / fleet routing |
| Cross-accelerator reroute-or-reject | CPU fallback / degraded serving |
| Default-model / smart routing | Default-model routing — explicit-only for now |
| OpenAI-API-compatible clients | Vendor-specific clients / protocols |

## 6. Clients

All **OpenAI-API-compatible** clients. pi-agent is one example, not the design
target — the interface is defined for *any* such client. (Open question: which
concrete clients — a chat UI like OpenWebUI/NextChat, scripts, …? Affects only
what we test, not the API surface.)

## 7. Workload envelope

- **Present:** 2–3 small-to-mid LLMs running concurrently, ~2.6B to ~35B
  parameters, quantized across a 32 GB V100 + integrated graphics.
- **Similar (horizon):** a few more models of comparable size; a bigger single
  model that may need sharding across GPUs. *Image gen is out of this envelope
  (parked in §5).*

## 8. Key open questions (load-bearing)

- **[CONFIRMED] Front = Olla, not Ollama.** Olla fronts one `llama-server` per
  GPU. Ollama the chat app is not part of this design.
- **[QUESTION] AMD is "in scope" but not yet deployed.** "Any NVIDIA/Intel/AMD"
  names ROCm as a first-class backend, but only CUDA + SYCL exist today. Is an
  AMD box a near-term target or a horizon marker?
- **[QUESTION] Reroute semantics.** With static config, "reroute" only fires if
  a *duplicate* backend serves the *same* model. If a model lives on one
  accelerator and that's down, Olla must **reject** (it can't serve `ornith-*`
  on the SYCL backend). Confirm this model-dependent reroute/reject is the
  intended behavior, and whether a duplicate/failover backend is ever wanted.
- **[QUESTION] Static config editing.** Adding a backend currently means
  hand-editing `config.yaml` + a unit. Any desire to make backend registration
  data-driven (from the backend table) rather than manual?
- **[VERIFY] Exact model ids.** Config comment says `ornith-35B`; elsewhere the
  model is `ornith-1.5-27B`. Confirm the canonical reported names Olla matches
  on.
- **[QUESTION] Image later — how unified?** When image lands, is it behind the
  same Olla front (Olla is weak at image) or a second lane behind a gateway?
  Deciding now avoids a rework.

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
- An OpenAI-API client can request any model by name and get a response via Olla.
- When a backend that serves a model is down, the request **reroutes (to a
  serving duplicate) or rejects** — never silently falls back to CPU.
- Adding a new-vendor backend (e.g. AMD ROCm) is a backend-table row + one
  static endpoint, not a redesign.
- The same setup script runs on a box with different hardware and adapts.

## 11. Risks

- **Static Olla routing is brittle.** Model-name matching + hand-edited
  `config.yaml` is fragile as the backend table grows; a config typo takes the
  whole public front down.
- **Failover is model-dependent, not backend-dependent.** With one backend per
  model, "reroute" rarely fires — most single-backend outages just **reject**.
  Decide whether a duplicate/failover backend is worth provisioning.
- **Olla version pin.** Routing relies on Olla v0.0.29's `openai-compatible`
  profile (the `llamacpp` native `/v1/models` parser is broken on this
  llama.cpp build). A major Olla upgrade could change routing behavior.
- **Concurrency capacity.** A 27–35B and a 2.6B on one 32 GB card constrain how
  many concurrent models fit; placement/priority policy matters.
