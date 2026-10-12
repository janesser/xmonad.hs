# stable-diffusion.cpp (sd.cpp) — diffusion via GGUF

Status: **new direction, adopted 2026-10-08.** sd.cpp is now the preferred
image-generation path for this box, **replacing the vLLM-Omni + Qwen-Image-2512
probe** (see `vllm-omni-2512-probe.md`). It sidesteps the two things that sank
that probe — the ~57 GB bf16 load transient and the ~33 GB HuggingFace
download — by loading a GGUF straight to the card.

Related:
- `qwen-image-21.md` — the earlier Qwen-Image-2.1 work on the 6 GB box (kept).
- `handover-continue-on-bigger-box.md` — finish 2.1 on ≥16 GB VRAM.
- `vllm-omni-2512-probe.md` — why the vLLM-Omni path was abandoned for image-gen.

---

## Why sd.cpp wins here

| Axis | vLLM-Omni (abandoned for image-gen) | stable-diffusion.cpp |
|---|---|---|
| Load | bf16 **~57.5 GB** transient → OOM on 32 GB | **GGUF** on-disk quant, loads directly |
| Weights | ~33 GB streamed from HF at load | single GGUF file (+ VAE + text encoder) |
| Stack | PyTorch + diffusers bridge + version-mismatch noise | single C++ binary, deterministic |
| Backends | CUDA only | CUDA **and CPU** (CPU fallback on a small/fragile box) |
| Failure mode | 30-min stall, output buffered and lost | immediate C++ error / OOM |

The decisive point is the **load transient**: vLLM-Omni loads the bf16 DiT +
encoder into RAM/VRAM *before* quantizing, so even an "int8" run touches ~57 GB
and OOMs on the V100's 32 GB. sd.cpp's GGUF is already quantized, so a Q4 is a
~13 GB file that fits with headroom to spare.

## The one gotcha that travels with it: quant quality

sd.cpp supports Qwen-Image-2512 (support merged 2025-10-12;
`QwenImageTransformerBlock` in source), **but the common k-quants black out.**
`Q4_K_M`, `Q5`, `Q5_1`, `Q4_K` on Qwen-Image-2512 produce **black images** via an
activation-dequantization overflow (sd.cpp issues #1385 / #1158, lemonade #1411).
**Use `Q8_0`** (or another verified quant) — it is the safe, non-blanking option.
Q8_0 for Qwen-Image-2512 is ~26 GB — still fits on the 32 GB V100.

FLUX.1-dev is more quant-tolerant but is still best run at its **Q8_0 config**
(upcast CLIP to FP16, T5-xxl to Q8_0) — see the Civitai/gpustack quant table.

## Models, ranked for a 1024² render

| Model | sd.cpp | GGUF size | Text encoder | 1024² | Best on |
|---|---|---|---|---|---|
| **SDXL** | yes (long-standing) | ~5–6 GB Q8 / ~3 GB Q4 | CLIP (~0.5 GB) | native | **robustness probe** — no quant issues, bulletproof |
| **FLUX.1-dev (12B)** | yes (Oct 2025) | ~6–8 GB Q4 / ~11 GB Q8_0 | **CLIP + T5-xxl (~3 GB)** | native | best quality that fits |
| **FLUX.2-klein (4B)** | yes (recent) | ~2.6 GB Q4 | small | yes | speed + good quality, light on card |
| **Qwen-Image-2512** | yes (Oct 2025) | ~13 GB Q4 / ~26 GB Q8_0 | Qwen2.5-VL-7B | native | original target; **Q8_0 only** (Q4 blacks) |
| SD 1.5 | yes | ~2 GB Q8 | tiny CLIP | ~512–768 | trivial "does the binary run" check |

**Recommended progression for the test-bed:**
1. **SDXL** first — 1024² native, tiny, no quant gotchas. Proves the toolchain
   and the 1024² denoise→VAE→text-encoder path on the real card with zero
   fragility. Fast failure feedback.
2. **FLUX.1-dev Q8_0** as the "heavy pipeline" confirmation — stronger output,
   still well under 32 GB.
3. **Qwen-Image-2512 Q8_0** only if you specifically want the original target —
   the black-image quant risk makes it the least robust of the three.

## V100-specific notes (cyberkleiber, 32 GB, sm_70)

- **Prefer GGUF Q4/Q5/Q8 over FP8.** The V100 is sm_70 with **no FP8 tensor
  cores** — FP8 weights are software-emulated and *slower*. sd.cpp's GGML quants
  run fine via CUDA, so Q4/Q8 is the right lever.
- **Text encoders are the hidden cost.** FLUX (CLIP + T5-xxl) and Qwen-Image
  (Qwen2.5-VL-7B) each need a ~3 GB VL encoder *plus* CLIP + VAE — the "three
  components" (diffusion GGUF + VAE `safetensors` + text encoder GGUF). SDXL's
  single CLIP is why it's the low-friction probe.
- **Fit is never the blocker on 32 GB.** Even Qwen-Image-2512 Q8_0 (~26 GB) +
  encoder + VAE leaves headroom. This is the opposite of the vLLM-Omni run.

## RTX 2060 Mobile notes (lincopta, 6 GB, Turing sm_75, no FP8)

- Same rules apply, tighter. **SD 1.5 Q8 (~2 GB) and SDXL Q4 (~3 GB)** are the
  models that fit on 6 GB; FLUX/Qwen-Image (≥13 GB GGUF) do not — offload to CPU
  (sd.cpp `-o cpu`) works but is slow.
- sm_75 has no FP8 hardware either, so GGUF Q4/Q8 is again preferred over FP8.
- sd.cpp is the right tool here precisely because it runs on **CPU as fallback**
  (the 6 GB box's external NTFS drive also faults mid-run — see
  `handover-continue-on-bigger-box.md`).

## Setup

Build sd.cpp (CUDA) once:
```bash
git clone https://github.com/leejet/stable-diffusion.cpp
cd stable-diffusion.cpp && cmake -B build -DCMAKE_BUILD_TYPE=Release -DSD_BUILD_GPU_DRIVERS=ON
cmake --build build --config Release -j
```
(Prebuilt `sd-cli` / `sd-cpp` binaries also ship; build if you want CUDA.)

Pull the three components (example: FLUX.1-dev). GGUF from Civitai/gpustack /
HF (e.g. `gpustack/FLUX.1-dev-GGUF`); VAE and text encoders from the Comfy-Org
split tree:
```bash
hf download --local-dir . gpustack/FLUX.1-dev-GGUF        # diffusion Q8_0.gguf
hf download Comfy-Org/FLUX.1-dev-full-fp16 --subfolder vae # VAE safetensors
hf download Qwen/Qwen2.5-VL-7B-Instruct --subfolder encoder # T5-xxl (quantize to Q8_0)
```

Render (FLUX.1-dev, Q8_0):
```bash
./build/bin/sd-cli \
  --diffusion-model flux1-dev-Q8_0.gguf \
  --vae flux_vae.safetensors \
  --llm Qwen2.5-VL-7B-Instruct-Q8_0.gguf \
  --clip text_encoders/clip-vitalik-l16.safetensors \
  --prompt "..." --cfg-scale 2.5 --sampling-method euler \
  --steps 30 -H 1024 -W 1024 --flow-shift 3 --output out.png
```
For Qwen-Image-2512, same shape with `--diffusion-model qwen-image-2512-Q8_0.gguf`
+ `--llm Qwen2.5-VL-7B…-Q8_0.gguf` + the Qwen VAE; **do not use Q4.**

## Fast-failure probe recipe (early interception)

The whole reason for sd.cpp is that failures surface **immediately**, not after a
30-min timeout. Use it that way:
1. **Smoke** on SDXL Q8, 5 steps, 1024² — seconds; any C++ error is instant.
2. **Fit/quality** on the target model (FLUX Q8_0 / 2512 Q8_0) — watch peak
   VRAM with `nvidia-smi`; if it OOMs the error is immediate, no waiting.
3. Confirm a real (non-black) PNG is written before trusting a quant — the
   black-image bug is silent until you inspect output.

## Flux.2-klein-4b Q4_0 — smoke test fixed (2026-10-09)

The `flux-2-klein-4b-Q4_0.gguf` checkpoint now loads and renders end to end.
Smoke test (`sd-cli`, steps=20, guidance=4, seed=42, 512×512) produced a valid
non-black PNG. This was the first model successfully rendered through sd.cpp on
this box.

**Root cause — an atypical GGUF naming convention.** This quantizer's GGUF
stores its tensor names *without* the leading `model.diffusion_model.` prefix
that sd.cpp's version detection and model builders expect, and it carries no
`general.architecture` metadata. Loading it therefore failed first with
`get sd version from file failed` (`VERSION_COUNT`) and then, once detection was
bypassed, `tensor ... not in model metadata` — the tensors were all present,
just unprefixed.

**Fix — upstream PR #2122** (`leejet/stable-diffusion.cpp`):
- `model_loader.cpp` — detect Flux/Flux2 by model signature with or without the
  prefix, so `get_sd_version()` no longer returns `VERSION_COUNT` (logs
  `Version: Flux.2 klein`).
- `diffusion_engine.cpp` — detect an unprefixed Flux GGUF at load and apply the
  `model.diffusion_model.` prefix so the builders' tensor lookups match. The
  probe fires only on the Flux marker `double_stream_modulation_img` and only
  when the prefix is absent, so standard leejet GGUFs are untouched.
- `ggml_block.hpp` — dropped a `GGML_ASSERT` that required `weight_scale`
  element count of 1 or `out_features`; this Qwen3-4b-based encoder stores a
  weight_scale with an element count outside that range (the dequant path
  already handles any count).

The smoke-test command and log are kept local (per CONTRIBUTING.md, test
scripts stay out of the PR).

## Resolved: origin/main works with correct params — no code fix needed

**PR #2122 was closed by the maintainer (leejet, 2026-10-10) without merging.**
His resolution: the load failure was **not** a code bug — it was using `-m`/
`--model` instead of `--diffusion-model`. The fix is a usage fix, not a source
change, so `origin/main` loads `flux-2-klein-4b-Q4_0.gguf` correctly when the
standalone-diffusion flag is used.

**Verified against a fresh `origin/main` clone** (`d637cfb`, upstream renamed
`main`→`master`; our `98de9ab` fix is confirmed **absent**):
- `-m` / `--model` = *"path to full model"* (`model_path`)
- `--diffusion-model` = *"path to the standalone diffusion model"*
  (`diffusion_model_path`)
- Both live in `SDContextParams` (`examples/common/common.cpp`), which is
  registered in the CLI parse vector at `examples/cli/main.cpp:294`
  (`options_vec = {cli, ctx, gen}.get_options()`), so both are real, usable flags.
- `--diffusion-model` is what tells sd.cpp the GGUF is a standalone diffusion
  model whose tensor names already carry the `model.diffusion_model.` prefix —
  exactly the flux-2-klein layout. That is why the upstream binary loads it
  without PR #2122.

### Confirmed parameters in origin/main (all defined, none added by our PR)

Model components — `SDContextParams` (`examples/common/common.cpp`):

| Flag | Meaning | target field |
|---|---|---|
| `-m`, `--model` | path to full model | `model_path` |
| `--diffusion-model` | path to standalone diffusion model | `diffusion_model_path` |
| `--llm` | path to llm text encoder (e.g. mistral-small3.2 for flux2) | `llm_path` |
| `--tokenizer` | tokenizer.json path (repeatable) | `tokenizer` |
| `--vae` | path to standalone VAE | `vae_path` |
| `--vae-format` | auto,flux,sd3,**flux2**,wan (default auto) | `vae_format` |

Generation — `SDGenerationParams`:

| Flag | Meaning | default |
|---|---|---|
| `-p`, `--prompt` | the prompt to render | — |
| `--cfg-scale` | unconditional guidance scale | 7.0 |
| `--guidance` | distilled guidance (models with guidance input) | 3.5 |
| `--steps` | number of sample steps | 20 |
| `-H`, `--height` | image height, px | 512 |
| `-W`, `--width` | image width, px | 512 |
| `--flow-shift` | shift for Flow models (SD3.x/WAN) | auto |

Plus `-o`, `--output` (write path) in `examples/cli/main.cpp:67`.

**Correct smoke command (origin/main, no patch):** use `--diffusion-model`
instead of `--model`:

```bash
./build/bin/sd-cli \
  --diffusion-model "$MODEL" \
  --llm "$TE" \
  --tokenizer "$TOK" \
  --vae "$VAE" --vae-format flux2 \
  --prompt "..." --steps 20 --guidance 4 --seed 42 \
  -H 512 -W 512 --output out.png
```

**Note for the build:** `origin/main` is a git submodule layout — the `ggml`
submodule (`https://github.com/leejet/ggml.git`, pinned `d25b121`) must be
`git submodule update --init` before cmake configures (a shallow `--depth 1`
clone has it missing, causing `SD_GGML_SOURCE_DIR` errors). The
`examples/server/frontend`, `thirdparty/libwebp`, `thirdparty/libwebm` submodules
are not needed for the CLI/CUDA build.

- [x] Build sd.cpp (CUDA) on the target box and smoke SDXL Q8 at 1024² — done.
  Also confirmed with FLUX.2-klein-4b Q4_0 **on clean origin/master** (d637cfb,
  no patch): `--diffusion-model` → `Version: Flux.2 klein`, Qwen3-4b text
  encoder loads (the PR #2122 `weight_scale` assert does **not** fire), valid
  non-black PNG (CPU smoke run, mean luminance 64.6). PR #2122 was unnecessary.

### Empirical verification (2026-10-12)

Recompiled a clean `origin/master` build (`master-959-d637cfb`, `--DSD_CUDA=ON`,
118 ggml-cuda objects) after resetting `~/projs/stable-diffusion.cpp` off the
local `fix/flux-2-klein-gguf-support` branch. Ran on CPU (ornith held the V100)
with `--backend cpu --diffusion-model …`: loaded all three components,
`get_learned_condition` in ~50 s, sampler produced `out_originmain_cpu_smoke.png`
(256×256, exit 0, valid non-black). Confirmed: upstream main works for
flux-2-klein **when `--diffusion-model` is used** — the fix was a usage fix, not
a source change.
- [ ] Pick a box: V100 (32 GB, FLUX/2512 Q8_0) or RTX 2060 Mobile (6 GB, SDXL/SD1.5).
- [ ] For Qwen-Image-2512: confirm a non-blanking Q8_0 GGUF before committing.
- [ ] Pin GGUF providers (Civitai/gpustack / `unsloth/*-GGUF`) and verify blob integrity.
