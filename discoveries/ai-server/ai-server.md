---
title: Local Multi-Accelerator AI Server (omni-modality: text + audio + image + video)
status: draft
created: 2026-09-25
updated: 2026-10-03
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

This box has **one** useful accelerator (the 32 GB V100) but needs to run more
than one *kind* of workload on it — a fast coding backend (ornith/llama.cpp),
and omni-modal text+audio+image+video (vLLM-Omni). The whole point of the
design is **managing that one accelerator across competing workloads**. Three
reasons, in order:

1. **Run different workloads on limited local hardware (the reason it exists).**
   One V100, several models, none of them free to run at once. The V100 is the
   scarce resource and the box's real value is modern, fast, omni-modal serving —
   not a small ggml model crawling on the Iris Xe iGPU. LocalAI half-exercised
   this: its only GPU model ran on the DG1 at ~60 tok/s (5–6 min/agent round),
   never touched the V100, and did no real audio/video. The need was always
   **fast V100 serving**, and that's what this is built around.
2. **Seamlessly switch models / backends.** Because only one backend can hold
   the V100 at a time, switching between them has to be a deliberate, single
   command (`switch-ai-backend.sh`), not a fumble. The swap is the operational
   heart of the setup.
3. **A stable integration frontend that knows what can be switched in.** A
   single, well-known entry point (OpenAI-API-compatible) that clients can rely
   on — and that is *aware of the portfolio*, knowing which workloads exist and
   being able to route to whichever is live. Integration stays simple even though
   the backend behind it changes. (Which concrete thing plays this role is still
   an open candidate question — e.g. Olla is a candidate, unproven for the job;
   not yet decided.)

The rest of this section is the technical shape that serves those reasons.

- **Front + engine = vLLM-Omni** (`vllm-project/vllm-omni`), the OpenAI-compatible
  server *and* the serving engine. vLLM ships its own OpenAI-compatible
  `/v1/*` endpoint, so **no LocalAI** are needed — the server *is* the front. Not
  Ollama, not llama.cpp directly. This directly backs reason 3: vLLM-Omni is one
  more stable, OpenAI-compatible workload that the frontend can point at — no
  bespoke front to stand up for it.

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

- **[RESOLVED — buildable, not a hardware dead-end] vLLM-Omni CAN run on the
  V100 (sm_70).** 2026-10-03: disproved the earlier "impossible" verdict. The
  prebuilt vLLM 0.30 wheel IS compiled for sm_7.5+ and fails on the V100 with
  `no kernel image for device` / `libcudart.so.13`, but that's a *wheel*
  problem. The V100 is CUDA Compute Capability **7.0** (Volta); nvcc here is
  12.4, and **torch's cu126 line still ships sm_70 kernels**. So:
  - **Build vLLM 0.30 from source against torch cu126** with
    `TORCH_CUDA_ARCH_LIST=7.0`. `cuobjdump -elf` on the resulting
    `_C_stable_libtorch.abi3.so` confirms `arch = sm_70`; torch cu126 matmul
    runs on the real V100; vLLM imports and loads the sm_70 extensions cleanly.
  - Older vLLM (0.20+) already needs torch ≥ 2.11 (cu128, sm_70 gone), so the
    from-source cu126 build is the *only* path — V100 forks
    (`1CatAI/1Cat-vLLM`, `jajmangold/vllm-sm70`) are not vLLM-Omni-compatible
    and wouldn't give us audio/image/video anyway.
  - **Conclusion:** the V100 is fine; what was needed is a **from-source
    sm_70 build**, not a new card. Build recipe + gotchas live in §11d. The
    remaining open items are operational (see below), not hardware.
- **[BUILD GOTCHAS — the cu126 build works if you do these right].** Not
  blockers, but they cost time if you hit them cold:
  - `pip` is **not** in the uv venv — use **`uv pip install`** everywhere.
  - vLLM's `setup.py` needs build-only deps present with
    `--no-build-isolation`: **`setuptools_rust`, `setuptools_scm`, `jinja2`,
    `packaging`, `wheel`, `cmake`, `ninja`**, and
    `"setuptools>=77,<81"` (else `ModuleNotFoundError: setuptools_rust`).
  - Run `python use_existing_torch.py` on the vLLM source to strip the
    `torch==` pins, so the build keeps the installed cu126 torch instead of
    pulling cu130.
  - The final multi-target compile `-j=8` **OOM-kills** (~step 240/331, exit
    137 = SIGKILL) on this 14 GB box — cap **`MAX_JOBS=3`**.
  - Easiest deploy: `python setup.py build_ext --inplace` (in vllm-src), then
    copy the built `*.so` over the wheel's in site-packages (drop-in; same
    0.30.x ABI, only the CUDA arch differs). Do **not** trust a `pip install -e`
    that times out mid-build; the in-place swap is the reliable path.
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
does need the cu126 torch path + build tooling in §11d (nvcc 12.4 is already
present on this box) — note that in the installer.)

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

> **vLLM-Omni is omni-only — it will not serve plain text-output models.**
> Verified 2026-10-03 (`StageConfigFactory.get_pipeline_config`, reads only the
> tiny HF `config.json`, no weights): a model is served only if its inferred
> `model_type` is in `OMNI_PIPELINES` or its HF `architectures` match an omni
> pipeline's `hf_architectures`. Pure-text types (`qwen2`, `llama`, `mistral`)
> are not in `OMNI_PIPELINES`, and no omni pipeline registers a pure text-gen
> architecture (the only `ForCausalLM` target, `MiMoV2ASRForCausalLM`, is an
> ASR submodule of the MiMo omni model). Text models print "not registered to
> an Omni pipeline" and get `pipeline=None` → unservable. Sanity check:
> `Qwen2.5-Omni-7B` → `pipeline=qwen2_5_omni`; `Qwen2.5-7B-Instruct`
> → `None`. Note the model *class* registry still merges base-vLLM text classes
> (`Qwen2ForCausalLM` is importable), but pipeline resolution is the real gate.
> So a text model needs plain `vllm` (not vLLM-Omni) **and** native HF
> safetensors — ornith's GGUF runs on neither.

### 11d. The sm_70 build recipe (cu126 torch)

The prebuilt vLLM 0.30 wheel targets sm_7.5+ and fails on the V100. Build the
engine from source against torch's **cu126** line (which still ships sm_70),
targeting `sm_70`:

1. Clone vLLM **0.30.0** source; run `python use_existing_torch.py` to strip the
   `torch==` pins so the build keeps the cu126 torch already installed.
2. Give the env the build-only deps `setup.py` needs with
   `--no-build-isolation`: `setuptools_rust setuptools_scm jinja2 packaging
   wheel cmake ninja` + `"setuptools>=77,<81"`.
3. `TORCH_CUDA_ARCH_LIST=7.0 MAX_JOBS=3 python setup.py build_ext --inplace`
   (`MAX_JOBS=3` — the default `-j=8` OOM-kills at ~step 240/331 on 14 GB RAM).
4. Copy the freshly built `*.so` from `vllm-src/vllm/` over the wheel's in
   site-packages (drop-in; same 0.30.x ABI). Do **not** trust a
   `pip install -e` that times out mid-build.

Verify with `cuobjdump -elf <so>` → `arch = sm_70`, and that `import vllm` +
`from vllm.platforms import current_platform` succeed without a
`libcudart.so.13` error (the cu126 torch bundles the right runtime). Uses
`uv pip install` throughout — the uv venv ships no `pip`.

### 11c. Open verification items (gate the rollout)

- **#1 — the sm_70 build + torchcodec (§8 item 1, §11d).** VALIDATED. vLLM 0.30
  0.30 from source against torch cu126 with `sm_70`; V100 at cap 7.0 loads
  (`_C_stable`), torchcodec resolves. The original `import vllm_omni` blocker
  (`libnvrtc.so.13`/`libcudart.so.13`) was **not** a CUDA-version mismatch —
  torch 2.13.0 *bundles* the CUDA 13 runtime under `nvidia/cu13/lib`; torchcodec
  just couldn't find it. Fixed by an `LD_LIBRARY_PATH` export in
  `.venv/bin/activate` (portable, `$VIRTUAL_ENV`-relative, lists torch/lib +
  nvidia/cu13,cuda_runtime,cuda_nvrtc). `import vllm_omni` now succeeds via
  `activate` alone. Residual: benign vLLM 0.30.0 vs vLLM-Omni 0.1.dev1+`gee8fdab1d`
  major/minor warning. End-to-end inference still needs a vLLM-format model
  (cache holds only GGUF) and VRAM freed from pi's llama backend (~31 GB on the
  V100). See §13.
- **Model fit + speed.** Confirm VRAM footprint and real tok/s for the chosen
  omni model on the V100; size KV cache + image/video stage weights.
- **Native-HF model provisioning.** Decide what to pull into the bind-mounted HF
  cache in native format (not gguf), and how to manage/size it.
- **Version pin.** Pin vLLM + vLLM-Omni refs once the sm_70 build validates.
- **Per-process boot units.** One detached `vllm` launcher per model; confirm
  the oneshot-detach pattern holds for vLLM.

## 12. Risks

- **sm_70 build is fiddly but solved.** The prebuilt vLLM wheel won't run on the
  V100; a from-source cu126 build with `sm_70` does (§11d). The OOM on `-j=8`
  and the missing `setuptools_*` build deps are the only real traps, and both
  are known. This is no longer a hardware stall — but it is a build you must get
  right before anything else.
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

## 13. Routing Olla + the single-V100 swap (vLLM-Omni wired in)

Olla (`:40114`) is the one OpenAI-compatible front end. It routes each request by
reported model id to whichever backend is live. vLLM-Omni is added as a third
static endpoint (`dot_config/olla/config.yaml`):

```yaml
# in discovery.static.endpoints, after the two llama.cpp endpoints:
- url: "http://127.0.0.1:8091"   # vLLM-Omni (restart-vllm-omni.sh) on the V100
  name: "vllm-omni"
  type: "openai-compatible"
  priority: 50
```

**The swap is the whole design.** The box has *one* V100 (32 GB); only one
backend can hold it at a time. The two live backends are:

| mode | backend | port | unit |
|---|---|---|---|
| `instruct` | llama.cpp CUDA (:8081, ornith) | :8081 | `llama-cuda.service` (boot default) |
| `omni`     | vLLM-Omni (:8091, Qwen2.5-Omni-7B) | :8091 | background process (`restart-vllm-omni.sh`) |

Swap with one command (the `sudo` is the NOPASSWD drop-in's `systemctl --system`):

```bash
switch-ai-backend.sh omni Qwen/Qwen2.5-Omni-7B   # -> free VRAM from ornith, launch vLLM-Omni
switch-ai-backend.sh instruct                    # -> stop vLLM-Omni, relaunch ornith on :8081
```

`restart-vllm-omni.sh start` activates the venv (which exports the
`LD_LIBRARY_PATH` that lets the torchcodec cu132 runtime find torch's bundled
CUDA-13 libs), probes the NVIDIA driver (fails fast instead of OOMing into RAM),
and forks `vllm serve … --omni --port 8091`, waiting on `/v1/models`.

**Why the swap is manual, not automatic:**

- **Olla is never restarted by the switch** — its `ExecStartPre` requires :8081
  reachable, so restarting while vLLM-Omni holds the V100 (no :8081) would fail.
  Olla picks up the new backend's models on its periodic discovery refresh
  (verified: with :8091 down, Olla stays healthy and keeps routing ornith).
- **Reaping the llama backend does NOT umount the shared HF cache** bind mount
  (`~/.cache/huggingface/hub` ← `/media/sailor/huggingface-hub`, fstab); it has
  to stay mounted for vLLM-Omni. The reaper is port-bound, so it only kills the
  `:8081`/`:8091` process, never a sibling backend.
- **pi's llama backend is ornith on the same V100** — starting vLLM-Omni stops
  ornith, so during `omni` mode pi has no coding backend. Do the swap deliberately.

vLLM-Omni is **omni-only** (§11b note) — it cannot serve the plain-text models
that instruct mode uses, which is exactly why the swap exists.

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

### D. vLLM-Omni pivot — handover for a new session (2026-10-03)

**State: Olla wiring DONE · model downloading · activation pending your go-ahead.**

#### ✅ Done
- **torchcodec blocker RESOLVED.** `import vllm_omni` failed on `libnvrtc.so.13`/
  `libcudart.so.13`; torch 2.13.0 *bundles* the CUDA-13 runtime under
  `nvidia/cu13/lib` — torchcodec just couldn't find it. Fixed with a portable
  `LD_LIBRARY_PATH` export in the venv activate (`/media/sailor/ai-server/.venv/bin/activate`,
  lines ~132-137). `import vllm_omni` now succeeds; sm_70 V100 load verified. The
  only residual is a benign vLLM 0.30.0 vs vLLM-Omni dev-version warning.
- **Olla wired for vLLM-Omni.** Added the `vllm-omni` endpoint
  (`127.0.0.1:8091`, `openai-compatible`, priority 50) to
  `dot_config/olla/config.yaml`; Olla stays healthy with it down and picks up the
  omni models on its discovery refresh. Committed.
- **Lifecycle scripts** (in `~/.local/bin/`, rendered from `dot_local/bin/`):
  `restart-vllm-omni.sh` (launch/stop vLLM-Omni on the V100) and
  `switch-ai-backend.sh {omni|instruct}` (swap which backend holds the single V100).
  Documented in **§13**.
- **Model fit decided:** Qwen2.5-Omni-7B is **24 GB** of fp16 weights → won't fit
  a 32 GB V100 once KV cache is added. Target is **Qwen2.5-Omni-3B (~12 GB)** —
  comfortable headroom for stages + KV. Both are vLLM-Omni-supported.

#### 🔄 In progress
- **Qwen2.5-Omni-3B download** running detached (`hf download … --local-dir`),
  landing in `/media/sailor/ai-server/models/Qwen2.5-Omni-3B/` (NOT the HF cache).
  Check completion by counting weight shards:
  `…/models/Qwen2.5-Omni-3B/*.safetensors` → expect **3**. Current: ~8/12 GB.
- V100 still holds **ornith** (pi's llama backend, :8081, ~31 GB). Olla active.

#### ⏳ Pending (next session)
1. **Confirm download done** — 3 `.safetensors` shards present.
2. **Activation swap** — *your call* (see ⚠️ below):
   ```bash
   switch-ai-backend.sh omni /media/sailor/ai-server/models/Qwen2.5-Omni-3B
   ```
   This stops ornith (frees VRAM) and launches vLLM-Omni on :8091; Olla picks it up.
3. **Live omni test** — hit `/v1/chat/completions` on :8091 with text + audio +
   image inputs to prove sm_70 kernel launch end-to-end. Then update §11 #1 from
   *feasible* to *validated* and record real tok/s + VRAM footprint.
4. **Optional:** LocalAI teardown (§13 → thread C): stop/disable the `localai`
   user unit, remove the Olla layer + `ola` autodetect provider +
   `update-localai-hf-sources.sh` — **only after** vLLM-Omni is standing, since
   Olla is now vLLM-Omni's front end, not LocalAI's.

#### ⚠️ Gotchas (hard-won — re-read before touching anything)
- **One V100, one backend at a time.** ornith and vLLM-Omni fight for the same
  32 GB; `switch-ai-backend.sh` manages the swap. Never start both.
- **pi's llama backend is ornith on the V100.** Activating vLLM-Omni stops it →
  pi has no coding backend during `omni` mode. Do the swap deliberately.
- **`hf download` is broken in cache mode** (prints the snapshot path, fetches
  nothing). Use `hf download … --local-dir <path>`.
- **Do NOT restart Olla during `omni` mode.** Its `ExecStartPre` requires :8081
  reachable; with ornith down that restart fails. The swap relies on Olla's
  periodic discovery refresh instead.
- **The HF cache-prune script is safe now** — the download goes to a local dir,
  not `~/.cache/huggingface/hub`, so `run_9_cleanup_hf.sh` (which runs
  `hf cache prune -y` on every `cz apply`) won't touch it. (It was temporarily
  moved aside earlier while the 7B was in the cache; restored from git.)
- **Don't go unilaterally stopping pi's llama backend** — it's this session's
  active backend; confirm before the swap.

#### Where things live
| Item | Path |
|---|---|
| venv (vLLM 0.30.0, torch cu126, vllm_omni) | `/media/sailor/ai-server/.venv` |
| vLLM-Omni model download | `/media/sailor/ai-server/models/Qwen2.5-Omni-3B/` |
| launch/stop script | `~/.local/bin/restart-vllm-omni.sh` |
| swap script | `~/.local/bin/switch-ai-backend.sh` |
| Olla config | `~/.config/olla/config.yaml` (source: `dot_config/olla/config.yaml`) |
| Olla binary | `~/.local/share/mise/installs/github-thushan-olla/0.0.29/olla` |
| HF cache (bind-mounted) | `~/.cache/huggingface/hub` ← `/media/sailor/huggingface-hub` |

### E. Omni front-end / successor-consumer *(new, 2026-10-07)*

> **Status update (2026-10-08).** The image-gen probe ran and **failed on fit**: Qwen-Image-2512 is ~57.5 GB bf16 (40.9 GB DiT + 16.6 GB encoder) and vLLM-Omni's `int8` materializes bf16 before quantizing → OOM at load on the 32 GB V100. The **vLLM-Omni image-gen path is abandoned in favour of stable-diffusion.cpp + GGUF** (pre-quantized, fits with headroom; also sidesteps the ~33 GB HF download). Full write-up + clean-up checklist: [`vllm-omni-2512-probe.md`](../ai-image-gen/vllm-omni-2512-probe.md); sd.cpp direction and ranked model range: [`stable-diffusion-cpp.md`](../ai-image-gen/stable-diffusion-cpp.md). The probe order below is updated to the sd.cpp progression. (For the borrow: ornith's reaper tree was killed to free the V100 — restore with `sudo systemctl --system start llama-cuda` when Jan says so.)

**Gap (big picture):** the omni engine (§11) is multimodal — text/audio/image/video —
but the only wired front ends (Olla on :40114, pi-agent's OpenAI text client) are
**text-biased.** The design assumed "engine = front," which holds for text but not for
omni: a plain OpenAI text client can never *render* an image, *play* audio, or *show*
video. So no client currently consumes the omni engine's non-text outputs. The
front-end/consumer decision (§6, §13 open item) is still unresolved.

**Increment — "successor-consumer": work on the front end that comes after pi-agent as
the consumer of the omni engine. Shape is deliberately **open** — no default lean
(agent-as-tools, chat UI, adapter, or CLI). The only hard requirement is that it
actually *consumes* the multimodal outputs, not just re-echoes text.

- **Why:** without a multimodal-aware consumer, the engine's differentiating value
  (audio/image/video) is never exercised end-to-end, however well vLLM-Omni serves on
  the V100.
- **What to build:** the minimal consumer that proves one **non-text in → non-text out**
  omni round trip is truly consumed (not just collapsed to a text caption).
- **Acceptance:** a client submits an audio or image input and receives an audio or
  video output that is *rendered/used* (played back, displayed, or fed to another tool)
  — not text-only.
- **Open decision this depends on (PM):** (1) primary use case — voice assistant vs
  image understanding/generation vs video vs general multimodal playground; (2) who's
  at the keyboard — pi-agent doing work vs a human at a UI; (3) front-end shape:
  multimodal chat UI / pi-agent-as-tools / thin adapter / CLI. Until (1)–(3) are
  decided, keep the increment shape-open and at "prove one multimodal round trip."

#### Direction decided (party, 2026-10-07 — Jan chose option 1)

The party (John PM orchestrating; Vex/Grumbal/Boundary/Yui/Dana/Wildcard/Level/
Killjoy/Splinter) resolved the shape. Outcome:

- **Path: image-generation first** (Jan's ladder: image-gen → listen & talk →
  listen/see & talk). Image-gen, not understanding, is the increment.
- **Candidate model — CHANGED: use `Qwen/Qwen-Image-2512`, NOT 2.1.** Probe
  2026-10-07 (recognition, weight-free) was **conclusive and caught a blocker
  before any download:**
  - `Omni(model=…)` resolves the pipeline via `model_index.json` → `_class_name`,
    looked up in `DiffusionModelRegistry`; an unregistered class raises
    `ValueError("Model class … not found in diffusion model registry")` (registry
    registry.py:504) *before* generation.
  - `Qwen/Qwen-Image-2.1` → `_class_name = QwenImage21Pipeline` → **zero references
    in our vLLM-Omni source → unsupported.**
  - `Qwen/Qwen-Image-2512` (and base 1.0) → `_class_name = QwenImagePipeline` →
    **registered** → supported. 2512 is the newer supported model in the same
    family; 2.1 needs a vLLM-Omni upgrade to register its class (heavy/risky,
    given the existing vLLM 0.30 vs vLLM-Omni 0.1.dev1 mismatch) or a speculative
    `--model-class-name QwenImagePipeline` override (2.1's architecture differs).
  - So: **target = Qwen-Image-2512, 20B MMDiT, INT8 (~16 GB).**
  - Qwen2.5-Omni-3B (already on the box) *understands* images; it does not
    *generate*, so image-gen needs this separate diffusion model, not the 3B.
- **Architecture — option 1: time-slice, not space-slice.** Qwen-Image is a
  **third V100 occupant** behind the existing `switch-ai-backend.sh` (ornith ↔
  omni ↔ Qwen-Image). Ornith runs normally until you need images; then the V100 is
  *borrowed* for the generation and ornith swapped back. Jan explicitly rejected
  option 2 (permanently demoting pi's coding to the Iris Xe SYCL 0.8B on :8082).
- **Hard constraints (from a live `nvidia-smi`/`free` check 2026-10-07):**
  - V100 must be **freed entirely** — ornith holds ~27 GB (86 %); 27 + 16 > 32, so
    no coexistence. Qwen-Image runs on a *dedicated* V100.
  - **CPU offload is NOT viable** — only 14 GB system RAM, ~5.6 GB free (swap 2/4 GB
    used). INT8 Qwen-Image must fit in the V100 alone; offload can't be the escape
    hatch it is on bigger-RAM boxes.
  - pi's fallback coding backend during the borrow is the **0.8B on Iris Xe SYCL
    (:8082)** — a working-but-flimsy backend (known runaway-loop quirk).
- **Prefer the offline per-gen path** (`text_to_image.py`), not a long-running
  Qwen-Image server. Per-gen load→generate→unload makes the V100 borrow transient,
  which is what keeps option 1 pleasant (ornith available between jobs).

**Decomposed increment (ordered by risk; do the probes before building anything):**

1. **Gate probe — fit (first).** On a dedicated V100, does the pipeline render a
   1024² PNG and fit? Run via **stable-diffusion.cpp + GGUF, not vLLM-Omni** — the
   vLLM-Omni fit probe (2512, `--quantization int8`) failed on load (~57.5 GB bf16
   transient vs 32 GB), so sd.cpp is now the gate; it also fails *immediately* (no
   30-min stall), which is the point. Progression: **SDXL Q8** smoke (1024², no
   quant gotchas → proves the toolchain) → **FLUX.1-dev Q8_0** (heavy, best
   quality) → **Qwen-Image-2512 Q8_0** (original target; Q8_0 only — the
   k-quants black out). See `stable-diffusion-cpp.md`. (The older vLLM-Omni order
   — recognition then fit via `text_to_image.py` — is recorded as abandoned in
   `vllm-omni-2512-probe.md`; do not re-run it as written.)
2. **pi failover probe.** When ornith (`:8081`) drops on the swap, does the crossbar
   **auto-route pi to the SYCL 0.8B**, or does pi's coding break? If no, option 1
   degrades to "no coding backend during a generation" — same as omni mode. This is
   a UX gap, not a VRAM gap, and it's the one unverified thing in the plan.
3. **Swap wiring.** A launcher mirroring `switch-ai-backend.sh`: free the V100 from
   ornith, run the offline generation, swap ornith back. 90 % a copy of the existing
   script once probes 1–2 pass.

**Still open:** crossbar auto-failover to SYCL on swap (§13 / crossbar config);
confirm `:8091`-style online serving for Qwen-Image is *not* needed unless Jan later
wants a person-facing ComfyUI front (he hasn't).
