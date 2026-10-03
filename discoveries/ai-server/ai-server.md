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
