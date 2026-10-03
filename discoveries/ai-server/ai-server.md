---
title: Local Multi-Accelerator AI Server (omni-modality: text + audio + image + video)
status: draft
created: 2026-09-25
updated: 2026-10-02
---

# Local Multi-Accelerator AI Server — omni-modality

> **Replaces / obsoletes** `discoveries/ai-llama-autostart.md`. That doc's scope
> was "make llama.cpp's single server boot reliably."
>
> **Oso obsolete:** this revision **retires the LocalAI runtime model** from the
> previous `discoveries/ai-server.md`. LocalAI is ggml-focused and small-model;
> on this box it ran a 0.5 B model on the slow Iris Xe iGPU at ~60 tok/s
> (5–6 min/agent round), never touched the V100, and does not do real audio or
> video. The pivot here is to **vLLM-Omni** — the omni-modality serving framework
> that extends vLLM's high-performance text runtime to audio, image and video.
> See §11.

## 1. Purpose (one line)

A single-box local AI server that front-end exports the **NVIDIA V100** through
an **OpenAI-API-compatible server**, runs the **current generation omni-modality**
models, and serves **text, speech/audio, image and video** from a single
engine — sized for a small concurrent portfolio.

## 2. Why this exists

- The previous **LocalAI** thread was deployed and half-running, but the
  experience was poor: the only GPU-backed model lived on the **Iris Xe DG1**
  (SYCL, ~60 tok/s — a full agent round took 5–6 min), the V100 was never
  actually used for serving, and the stack could only do small GGUF text models
  (no real audio/video; "image" was a thin GGUF diffusion path).
- The real need is **modern, fast, omni-modal serving**, not "small ggml model
  on the iGPU." vLLM-Omni gives true GPU acceleration on the V100 **and** audio +
  image + video — what LocalAI never provided.
- **Front + engine = vLLM-Omni** (`vllm-project/vllm-omni`), the OpenAI-compatible
  server *and* the serving engine. vLLM ships its own OpenAI-compatible
  `/v1/*` endpoint, so **no LocalAI and no router** are needed — the server *is*
  the front. Not Ollama, not Olla, not llama.cpp directly.

## 3. Architecture (the target topology)

```
        OpenAI-API clients (pi-agent + chat UIs + scripts)
                         │  [::]:<port>  (LAN)
                         ▼
               ┌───────────────────┐
               │   vLLM-Omni ×N    │  one server process per model,
               │  OpenAI-compatible │  OpenAI-compatible API, --omni
               └───────┬───────────┘
                       │
                 NVIDIA V100 (V100-SXM2-32GB, sm_70)
                       │
            HF native weights (diffusers/hf, NOT gguf)
            (~/.cache/huggingface/hub bind-mounted)
```

- **vLLM-Omni** = the OpenAI-compatible server (its own `/v1/*`, including chat,
  completions, and omni audio/image/video endpoints). It is the front *and* the
  engine — the LocalAI and Olla layers dissolve.
- **One server process per model.** vLLM serves the model named on its command
  line; to host a portfolio you run **one `vllm`/`vllm serve` process per
  model** (each pinned to the V100). Multiple models = multiple server
  processes, each an independent, isolated instance. (The "one instance, dynamic
  routing" model of the LocalAI doc does **not** carry over — vLLM is one model
  per process; see §8.)
- **Per-model boot units** (`etc/systemd/system/`): each is `Type=oneshot`
  whose launcher detaches `vllm` (fork + disown); systemd tracks only the
  launcher. `KillMode=process`; `Restart=on-failure`.
- **Fail-fast, no CPU fallback:** vLLM refuses to start unless a CUDA device is
  present. `media-passeport` automount + huggingface-hub bind-mount in
  `/etc/fstab` means no `sudo mount` at boot.

## 4. Requirements

### Functional
- **vLLM-Omni as OpenAI-compatible front + engine.** A single well-known
  serving engine (vLLM-Omni) that *is* the OpenAI-compatible front. No LocalAI,
  no router.
- **One vLLM process per model; V100-bound.** Every model runs on the V100 via
  vLLM's CUDA runtime. Adding a model = another server process, no redesign.
- **Omni-modal outputs.** Text/completion, speech/audio in and out, image and
  video — whichever the served model supports (see §11 for which models this
  box can actually host).
- **Explicit model selection.** Clients **name the model**. No default-model /
  smart-routing layer.
- **OpenAI-API-compatible front.** Any OpenAI-API client (pi-agent and others)
  works out of the box.
- **Reject on outage — never CPU fallback.** If a backend is unavailable, the
  request is rejected; never silently served from CPU/RAM.

### Non-functional
- **Fail-fast, visible.** A down accelerator/server surfaces loudly.
- **Portable.** Same setup script runs across boxes with different hardware.
- **Fast.** The whole point vs LocalAI: real token throughput on the V100
  (orders of magnitude faster than the 60 tok/s iGPU path), not a slow
  small-model demo.

## 5. Scope

| In scope | Out of scope (for now) |
|---|---|
| Text/completion via vLLM-Omni (V100, one process per model) | DG1 / second-accelerator — dormant, not targeted by vLLM-Omni |
| Omni audio/speech, image, video — per model | All models at once on one process (vLLM is one model/process) |
| **V100 CUDA serving** (the real goal; localizes the sm_70 risk) | Intel SYCL / CPU serving as a fast path |
| Native HF weights (diffusers/hf), not gguf | LocalAI's gguf model wiring |
| OpenAI-API-compatible clients | Vendor-specific clients / protocols |
| Concurrent portfolio = multiple isolated vLLM processes | One mega-model; GPU sharding |

## 6. Clients

All **OpenAI-API-compatible** clients. pi-agent is one example, not the design
target. (Open question: which concrete clients — a chat UI, scripts, …? Affects
only what we test, not the API surface.)

## 7. Workload envelope

- **Present target:** 1–3 omni models of comparable size running concurrently on
  the V100 — e.g. a 7 B omni model, or a 30 B **A3B MoE** omni model (only ~3 B
  active params, so fast decode). Qwen2.5-Omni-7B (~14 GB FP16) and
  Qwen3-Omni-30B-A3B (Q8 ~16–20 GB) both fit in the 32 GB V100.
- **Similar (horizon):** a few more models; the MoE ones stay fast because only
  their active parameters run.

## 8. Key open questions (load-bearing)

- **[CONFIRMED BLOCKER] vLLM-Omni cannot run on the V100 (sm_70), period.**
  Resolved 2026-10-02 — this is not an open gate, it's a hard hardware
  incompatibility:
  - The V100 is CUDA Compute Capability **7.0** (Volta); nvcc here is 12.4.
  - **vLLM-Omni requires vLLM 0.30** (its rule: "same major.minor vLLM as
    vLLM-Omni").
  - **vLLM 0.30 hard-pins `torch == 2.13.0`**, whose wheels are **CUDA 13.0
    with sm_70 dropped** (CUDA 13.0 itself dropped Volta). No vLLM 0.30 wheel,
    and no clean from-source build (nvcc 12.4 can't target sm_70 either), runs
    on this card.
  - Older vLLM (0.20+) already needs torch ≥ 2.11 (cu128, sm_70 gone), so **no
    vLLM-Omni version works on sm_70.** V100 forks exist (`1CatAI/1Cat-vLLM` →
    torch 2.10 + cu128; `jajmangold/vllm-sm70`) but they are **not
    vLLM-Omni-compatible**, so they don't give us audio/image/video.
  - **Conclusion:** vLLM-Omni needs an **Ampere+ (sm_80+) card.** The V100 is
    out for this engine. See the alternatives in §13 (hardware upgrade, or a
    narrower llama.cpp-on-V100 pivot).
  - This is why the whole pivot now hinges on a hardware decision, not a build
    knob.
- **[DECIDED] DG1 dormant again.** vLLM-Omni targets CUDA / ROCm / Intel Arc
  B-series / MUSA — **not** Iris Xe SYCL. The DG1 is out of scope; the V100 is
  the only serving accelerator. This also *removes* the old text<->image
  backend-switch fragility (#1498) — there is no ggml single-stack to break.
- **[QUESTION] One model per process → routing reintroduced?** vLLM serves the
  model named on its command line, so a portfolio means **multiple vLLM server
  processes** (one per model) that clients must reach. Options: (a) a client
  knows which port serves which model and speaks to each directly; (b) a thin
  router (LiteLLM / a small proxy / re-introduced Olla) in front for a single
  entry point and per-model port map. Decide whether the single-open-entry
  simplification is worth it vs. just pointing clients at the right port.
- **[QUESTION] Model management moves off gguf.** vLLM-Omni loads **native HF
  weights** (diffusers/hf layout), not the GGUF files the LocalAI thread wired
  from the HF cache. The existing cache-driven gguf wiring
  (`update-localai-hf-sources.sh`) is now irrelevant; the cache is ~212 GB and
  may not hold the needed models in the right format. Decide what to pull, into
  the bind-mounted HF cache, and how to manage/size it (omni models + their
  audio/image stage weights are large).
- **[VERIFY] Which omni models fit + are fast enough on the V100.** Concretely:
  Qwen2.5-Omni-7B vs Qwen3-Omni-30B-A3B (MoE, ~3 B active) vs MiniCPM-o 4.5 —
  VRAM footprint, quant, and real tok/s. Start with whichever fits comfortably
  with headroom for KV cache + image/video stage weights.
- **[OPEN] vLLM-Omni version pin.** vLLM-Omni rides the vLLM release line; pin
  the exact vLLM + vLLM-Omni refs once the sm_70 build is validated (§8 item 1).

## 9. Deployment

**Incidental, not product.** The deliverable is the server design + setup
script. Installation is whatever is convenient — chezmoi run script, plain
bootstrap, systemd unit — and must be **portable across hardware**. This brief
does **not** constrain the installer. (The sm_70 from-source build, §8 item 1,
does need a CUDA 12.6 toolchain + build tooling — note that in the installer.)

## 10. Success criteria

- vLLM-Omni runs a single omni model on the V100 (sm_70 build validated) and a
  client requests it by name over the OpenAI API.
- Text, and the model's audio/image/video outputs, are served end-to-end.
- Token throughput is real (not the ~60 tok/s iGPU demo) — a full agent round is
  minutes, not a multi-minute slog.
- A second model = a second isolated vLLM process that also serves over the
  OpenAI API.
- A down server rejects (never CPU fallback).
- The same setup script runs on a box with different hardware and adapts.

## 11. Runtime model — vLLM-Omni as OpenAI server + omni engine

**Decision (2026-10-02): vLLM-Omni is the front AND the engine.** It ships its
own OpenAI-compatible server, so the LocalAI front and the router both
dissolve. This is a cleaner architecture than the LocalAI doc: one well-known
serving engine instead of LocalAI + a lifecycle layer + a router.

### 11a. What vLLM-Omni gives (vs what LocalAI offered)

| Need | vLLM-Omni | LocalAI (retired) |
|---|---|---|
| Serve engine + OpenAI front | vLLM-Omni's own `/v1/*` server | LocalAI front + separate lifecycle/router |
| Acceleration | **V100 CUDA** (fast) | ggml; ran on **Iris Xe SYCL** (~60 tok/s) |
| Text/completion | ✅ (vLLM's forte) | ✅ (small models only) |
| Speech / audio in & out | ✅ (Qwen2.5/3-Omni, MiniCPM-o, TTS models) | ✗ |
| Image generation | ✅ (Qwen-Image, FLUX, GLM-Image, …) | thin GGUF diffusion path |
| Video generation | ✅ (Wan, LTX, Cosmos, …) | ✗ |
| Modern models | current omni / diffusion line | ggml small models |

### 11b. Supported omni text/audio models this box should consider

(from vLLM-Omni's supported-models list; image/video models are a separate,
larger list — see §11c):

- **Qwen3-Omni-30B-A3B-Instruct** — 30 B total / ~3 B active MoE; fits Q8 in
  32 GB; the strongest fit for speed+quality on the V100.
- **Qwen2.5-Omni-7B / -3B** — dense, ~14 GB FP16 (7B); comfortable headroom.
- **MiniCPM-o 4.5** — validated omni model.
- (LLaMA-3.1-8B-Omni and others appear via the broader vLLM line.)

Image/video generation models (Qwen-Image, FLUX.1/2, GLM-Image, Wan, LTX,
Cosmos, …) are supported too but carry large stage weights and push the 32 GB;
treat as horizon / a later model than the audio+text omni chat models.

### 11c. Open verification items (gate the rollout)

- **#1 — the sm_70 build (§8 item 1).** Resolve before anything else: from-source
  vLLM-Omni on CUDA 12.6, or pin vLLM ≤ 0.20. Without this the V100 can't run
  current vLLM at all.
- **Model fit + speed.** Confirm VRAM footprint and real tok/s for the chosen
  omni model on the V100; size KV cache + image/video stage weights.
- **Native-HF model provisioning.** Decide what to pull into the bind-mounted HF
  cache in native format (not gguf), and how to manage/size it.
- **Version pin.** Pin vLLM + vLLM-Omni refs once the sm_70 build validates.
- **Per-process boot units.** One detached `vllm` launcher per model; confirm
  the oneshot-detach pattern holds for vLLM.

## 12. Risks

- **sm_70 / V100 support is the top risk.** Current vLLM wheels don't ship sm_70;
  a from-source CUDA-12.6 build is required (or a ≤0.20 pin). If the build path
  proves fragile, this whole pivot stalls on hardware until a newer-arch card is
  in the box.
- **One model per process.** A portfolio is multiple isolated vLLM server
  processes; the single-open-entry simplicity of LocalAI is lost and a routing
  decision (§8 item 2) is needed if a single entry point matters.
- **Native-HF weights are large.** omni models plus their audio/image/video
  stage weights pull far more than GGUF and strain the bind-mounted cache — plan
  cache size and management.
- **Single V100 chokepoint.** One accelerator, no warm failover; reject-on-outage
  is the policy. A second accelerator would need a router (as above).
- **Concurrency capacity.** An omni model holding the V100 plus image/video stage
  weights can fill 32 GB; concurrency is model-dependent and must be tuned to
  actual sizes.
- **Client-side request timeouts.** vLLM serves; a long cold-load is the client's
  timeout concern, not the server's — make sure the OpenAI clients carry sane
  timeouts.

---

## Handover — state of the threads

### A. HuggingFace cache migration: `/media/passeport` sda1 → sdb1 (M.2)
*(unchanged from the previous doc — the sda1→sdb1 mirror/switch. See the prior
`ai-server.md` for the full status.)*

### B. GPU dual-driver: nouveau + nvidia_drm (GT 730)
*(unchanged — GT730→nouveau, V100→nvidia 580. See the prior doc.)*

### C. LocalAI deployment — RETIRED (2026-10-02)
The LocalAI thread (§11 of the previous doc) is **abandoned in favor of the
vLLM-Omni pivot** above. It was deployed and live (v4.10.0 on :8080, tiny model
on the Iris Xe SYCL), but slow, V100-unused, and text-only. **Teardown to run
when the vLLM-Omni path is standing up:** stop/disable the `localai` user unit,
remove the Olla layer and the `ola` autodetect provider / `OLLA_BASE_URL`, and
drop `update-localai-hf-sources.sh`. The gguf model wiring is now irrelevant —
vLLM-Omni uses native HF weights (§8 item 3).
