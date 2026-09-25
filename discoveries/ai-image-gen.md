# ComfyUI + Flux-2-Klein image generation (NVIDIA)

Status: **documented, script disabled pending solidify.** The command sequence
below was lifted from `.chezmoiscripts/run_once_5_aitools_4image_gen.sh`
(the `exit 0 # FIXME solidify` guard made it a no-op). It is recorded here as
handover documentation; the source script has been removed.

Related: other AI backend setup in `.chezmoiscripts/run_once_5_aitools_*`.

---

## ⏳ IN-PROGRESS TEST RUN — handover (2026-09-25)

A live run of **Qwen-Image-2.1** is in progress to produce a medical
illustration (human sitting up from a chair, lifting the upper body with both
arms). The weights are downloading **now** — do not delete the cache or the
`dl.sh` process.

**Current state**
- Weights downloading into the HF snapshot via `curl` (see `dl.log`).
  - Transformer shard 1/3 ≈ 9.3 GB (done); shard 2/3 downloading; 3/3 pending.
  - Then VAE (small) + text encoder (config, generation config, 4 × .safetensors).
- Snapshot: `~/.cache/huggingface/hub/models--Qwen--Qwen-Image-2.1/snapshots/790c92633540aa0cb11d9abf19eb46d861714758/`
- Working dir: `/tmp/qwenimg_test/` (scripts `dl.sh`, `encode.py`, `generate.py`).

**Why the quantized/CPU path (read before re-running)**
- **GPU:** RTX 2060 Mobile, **6 GB VRAM**, Turing (sm_75) — **no FP8** support
  (needs Ampere+). Model is ~31 GB bf16 (DiT ~14 GB + Qwen3-VL text encoder
  ~17.5 GB), so bf16/FP8 are both out.
- **Used:** int8 dynamic quant (`torchao Int8DynActInt8WeightConfig`).
  - Text encoder → **on GPU** (~4–7 GB int8, fits 6 GB).
  - DiT (~7 GB int8) → **on CPU** (exceeds 6 GB VRAM). VAE left bf16.
- **Network:** general internet is throttled (~20–40 KB/s). The
  **hf-mirror.com** mirror is the only fast path (~1–4 MB/s) and needs
  `HF_TOKEN` + `HF_ENDPOINT=https://hf-mirror.com`. PyPI/pytorch index are fast.

**Env**
- venv `~/.venvs/qwenimg`: torch 2.14 (+cu), diffusers/transformers from
  git main, torchao, torchvision. Activate: `source ~/.venvs/qwenimg/bin/activate`.

**Pipeline (after download completes)**
1. `cd /tmp/qwenimg_test && export HF_ENDPOINT=https://hf-mirror.com HF_TOKEN=$HF_TOKEN`
2. `~/.venvs/qwenimg/bin/python encode.py` → saves `embeds.pt` (GPU-compiled prompt embeddings).
3. `~/.venvs/qwenimg/bin/python generate.py` → DiT on CPU → `out.png`.

**⚠️ PATH BUG (this run)** — `dl.sh` computed the cache dir as
`models--Qwen/Qwen-Image-2.1` (a slash) instead of the canonical
`models--Qwen--Qwen-Image-2.1` (double-dash HF hub namespace separator). The
current download is healthy but writing to the wrong tree. Fix **once it
finishes**: merge `~/.cache/huggingface/hub/models--Qwen/Qwen-Image-2.1/`
into `~/.cache/huggingface/hub/models--Qwen--Qwen-Image-2.1/` (merge
`snapshots/` + `blobs/`), then delete the stray `models--Qwen/` dir. In
`dl.sh` hardcode that canonical dir name rather than building it from `$REPO`.

**Caveats on the result quality:** this is a lossy int8 + CPU-DiT smoke test;
expect softer detail than bf16/FP8. If the harness kills a job, the
`.incomplete`/partial snapshot files resume — just re-run `dl.sh`.

---

## TL;DR

Install **ComfyUI** locally on an NVIDIA box via `comfy-cli`, restore the
`Flux-2-Klein` model chain from HuggingFace, soft-link the files into the
ComfyUI `models/` tree, drop in the text-to-image workflow, then launch.

All of this is gated behind an NVIDIA-presence check so it never runs (or
installs anything) on a non-NVIDIA machine.

---

## 1. NVIDIA presence guard

The whole script skips itself unless an NVIDIA GPU is visible. Uses `lspci`
(not `lsmod`) because `lspci` needs no sudo and detects the GPU even when the
driver module is not loaded — `lsmod` would false-negative on a fresh setup.

```bash
if ! lspci 2>/dev/null | grep -iq nvidia; then
    echo "$(basename $0): No NVIDIA GPU detected, skipping..."
    exit 0
fi
```

---

## 2. Environment & comfy-cli

```bash
uv venv --allow-existing ~/.comfy
source ~/.comfy/bin/activate
uv pip install comfy-cli
```

---

## 3. ComfyUI setup & install

```bash
comfy setup --where local -y
cd ~/.comfy
comfy install --restore --nvidia --cuda-version 12.4 --fast-deps --version=latest

comfy manager enable-gui
comfy manager uv-compile-default true
```

- `--cuda-version 12.4` targets CUDA 12.4.
- `--restore` reinstalls the previously saved model chain.

---

## 4. Flux-2-Klein model download

Docs: <https://docs.comfy.org/tutorials/flux/flux-2-klein>

```bash
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/diffusion_models/flux-2-klein-base-4b.safetensors
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/diffusion_models/flux-2-klein-4b.safetensors

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/text_encoders/qwen_3_4b.safetensors
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/text_encoders/qwen_3_4b_fp4_flux2.safetensors

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/vae/flux2-vae.safetensors
```

(The two `diffusion_models` lines are duplicated in the original script.)

---

## 5. Soft-link downloaded files into ComfyUI models

Each file under the HuggingFace snapshot is soft-linked into the matching
`~/.comfy/ComfyUI/models/<subfolder>` directory:

```bash
for folder in ~/.cache/huggingface/hub/models--Comfy-Org--vae-text-encorder-for-flux-klein-4b/snapshots/*/split_files/*; do
    basefolder=`basename $folder`
    for item in `ls $folder`; do
        cd ~/comfy/ComfyUI/models/$basefolder
        ln -sf "$folder/$item"
    done
done
echo all linked nicely.
```

---

## 6. Workflow download

```bash
curl -o ~/comfy/ComfyUI/user/default/workflows/image_flux2_klein_text_to_image.json \
  https://raw.githubusercontent.com/Comfy-Org/workflow_templates/refs/heads/main/templates/image_flux2_klein_text_to_image.json
```

---

## 7. Launch

```bash
# comfy launch (command was not captured beyond this comment)
```

---

## 8. Qwen-Image-2.1 (new candidate — 2026-09-20)

Docs: <https://huggingface.co/Qwen/Qwen-Image-2.1> · <https://qwen.ai/blog?id=qwen-image-2.1> · repo <https://github.com/QwenLM/Qwen-Image-2.1>

A single unified text-to-image **and** image-editing diffusion model. Notable:

- **Compact** — 7B-param visual generation component (32 Single-Stream DiT layers); far lighter than the 6-file Flux-2-Klein restore above.
- **Native RGBA transparency** from text (generate or extract a subject on a transparent background) — directly useful for icons/schematics that need to drop onto any background.
- **Improved typography / text rendering** — relevant where labels/signs matter.
- **Editing** — up to 10 reference images, local edits via circles/painted annotations/masks, identity preservation for people/products.
- **Day-0 ecosystem** — diffusers `QwenImage21Pipeline` (`.bf16`→CUDA); **ComfyUI** native with workflow templates at `Comfy-Org/Qwen-Image-2.1`, templates `image_qwen_image_2_1_t2i.json` / `image_qwen_image_2_1_image_edit.json`; also vLLM-Omni, SGLang, LightX2V, ModelScope/DiffSynth-Studio, AMD ROCm. FP8 quant + CPU offload for small GPUs. NVIDIA/CUDA only for the base pipeline.

### Why it matters here

Single-model install + ComfyUI-native fits the existing `comfy-cli`/restore
workflow and would replace the multi-file Flux-2-Klein restore. The RGBA
output is a real advantage for icon/schematic generation. **Prompt-rewriting
checkpoints** (`Qwen/Qwen-Image-2.1-PE-T2I`, `-PE-I2I`, Qwen3.5-VL 9B) improve
results.

> **License caveat (read before generating anything for a product):** Qwen
> Research License = **non-commercial use only**; commercial use needs a
> separate license from the author. See `discoveries/ai-rehab-handout.md`
> for how this bites the rehab handout (Q3/14).

## Notes / open items

- The script was intentionally disabled (`exit 0 # FIXME solidify`) — the
  setup had not been hardened into an idempotent, reviewed run script.
- Qwen-Image-2.1 is a lighter, RGBA-capable alternative worth a try once a
  `--purge`-safe guard is in place.
- Consider adding back a `--purge`-safe guard: if no NVIDIA GPU is present,
  this flow never touches anything, but the broader AI tooling should not
  install `nvidia-cuda-toolkit` either (see the `lspci` pattern used in
  `.chezmoiscripts/run_once_3_devtools_3podman_nvidia_cdi.sh`).
