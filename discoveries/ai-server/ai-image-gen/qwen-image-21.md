# Qwen-Image-2.1 text-to-image (NVIDIA)

Status: **documented, generation smoke-test pending.** Weights downloaded and
the Python env is ready; the only unverified step is the actual `generate`
call under the 6 GB VRAM / 15 GB RAM constraint (see below). This is the
current direction — it supersedes the ComfyUI / Flux-2-Klein restore in
`comfy-flux-2-klein.md`.

Related: older ComfyUI/Flux approach `discoveries/ai-image-gen/comfy-flux-2-klein.md`.

---

## ✅ Current status — one step left (as of 2026-09-26)

Target output (from the original 2026-09-25 run): a medical illustration
(human sitting up from a chair, lifting the upper body with both arms).
Target sample / aesthetic: the Qwen-Image-2.1 Showcase (`#text-to-image`,
`example-01` neon-sign etc.) at
<https://huggingface.co/Qwen/Qwen-Image-2.1#text-to-image>.

- ✅ Weights **fully downloaded** into the canonical HF snapshot
  `790c92633540aa0cb11d9abf19eb46d861714758` (28/28 blobs, no broken symlinks,
  ~33.8 GB).
- ✅ Python env fully installed (see below).
- ⬜ **Generation not yet run.** The remaining problem is purely a VRAM/RAM
  squeeze (details + a working encode-first recipe below).

Cache (blobs + index):
```
~/.cache/huggingface/hub/models--Qwen--Qwen-Image-2.1/
├── blobs/          # the ~28 weight/config blobs (~33.8 GB total)
└── snapshots/      # the checked-out, gitlink-resolved tree
```

**Resolved snapshot (the tree the pipeline actually reads):**
```
~/.cache/huggingface/hub/models--Qwen--Qwen-Image-2.1/snapshots/790c92633540aa0cb11d9abf19eb46d861714758/
├── model_index.json    # pipeline config (points at the blobs)
├── transformer/        # the ~14 GB DiT (diffusion transformer)
├── text_encoder/       # Qwen3-VL text encoder (~17.5 GB bf16)
├── vae/                # ~1.4 GB VAE
└── processor/          # tokenizer/split_files — NOT at the repo root
```
This is the canonical snapshot (`790c926…`, 28/28 blobs, no broken symlinks).
Any of the subdirs can be pointed at directly, e.g.
`AutoTokenizer.from_pretrained(.../processor/)`.

Working dir: `/tmp/qwenimg_test/` (`step1_encode.py` works; the earlier
`generate.py` is a stale draft — replace it with the encode-first recipe below).

### Why the quantized/CPU path is required

- **GPU:** RTX 2060 Mobile, **6 GB VRAM**, Turing (sm_75) — **no FP8** support
  (needs Ampere+). So FP8 would run in software emulation and be *slower* than
  int8 here — see the quantization choice below.
- **Measured bf16 sizes (from disk):** DiT **14.23 GB** + Qwen3-VL text encoder
  **17.53 GB** + VAE **1.35 GB** = **33.1 GB** total. Both bf16 and FP8 are out
  of VRAM reach.
- **RAM:** 15 GB total. Even int8, text encoder (~8.8 GB) + DiT (~7.1 GB) ≈
  15.9 GB, so the two can't be resident at once.
- **Even quantized, nothing fits in 6 GB on its own:** an INT8 DiT is
  **7.26 GB** and a pre-cast FP8 encoder is **9.39 GB** — both exceed the card.
  So **CPU offload is mandatory** (not optional): weights swap and at any
  instant only one component's active layer + KV cache + activations is on the
  GPU, which stays well under 6 GB.

### Quantization choice — INT8, and where to get it

Available on Hugging Face:

- **`unsloth/Qwen-Image-2.1-FP8`** — recommended. Pre-quantized safetensors:
  INT8 (W8A8, "shipped" scheme, LPIPS 0.064) DiT **7.26 GB**, FP8 (Dynamic)
  DiT **7.12 GB**, pre-cast FP8 Qwen3-VL encoder **9.39 GB**, bf16 VAE 0.63 GB.
- `Rin247/Qwen-Image-2.1-*` — adds INT4 / FP4 weight-only (smaller, lossier).
- `ProCreations/Image-2.1-Calibrated-FP8` — independently calibrated W8A8 E4M3.
- Your current `Qwen/Qwen-Image-2.1` — bf16 (33.1 GB).

**Recommendation for this Turing box: INT8 + CPU offload.** sm_75 has no FP8
hardware, so FP8 is software-emulated (slower) and slightly lossier (0.112 LPIPS
vs 0.064 for INT8). INT8 is both faster and higher quality here.

INT8 is the torchao scheme already used in the encode-first recipe below
(`Int8DynamicActivationInt8WeightConfig` on the DiT,
`Int8WeightOnlyConfig` on the encoder) — so you can either quantize the bf16
yourself, or just `hf download` the pre-quantized unsloth files and point the
pipeline at them, skipping the encoder-encoding step entirely.

### Try the official small-GPU path first

The model page's recommended memory optimization for small GPUs is a one-liner
rather than the manual recipe below:

```python
pipe = QwenImage21Pipeline.from_pretrained(
    "Qwen/Qwen-Image-2.1", torch_dtype=torch.bfloat16
)
pipe.enable_model_cpu_offload()
```

`enable_model_cpu_offload()` swaps weights to CPU, so it never holds both
components on the GPU at once — try this before the manual encode-first flow.

> **Not viable on this box:** that one-liner loads the *full bf16* model to CPU
> RAM (~31 GB) before offloading, which exceeds the 15 GB RAM. It works on
> machines with more RAM. Here you need the **quantized** path — the INT8
> recommendation above (encode-first holds only one component at a time,
> well under 15 GB).

### Encode-first recipe (if CPU offload still OOMs on 6/15)

`from_pretrained` loads the text encoder + DiT + VAE **simultaneously** → OOM.
Recipe that avoids holding two ~7–9 GB models at once:

1. Load the text encoder (`Qwen3VLForConditionalGeneration`, model_type
   `qwen3_vl`) **int8 weight-only** via accelerate's memory-efficient dispatch
   (`init_empty_weights` + `load_checkpoint_and_dispatch(..., quantizer=...)`),
   on CPU. Encode the prompt → `prompt_embeds` (+ attention mask).
2. `del` the encoder, `gc.collect()` + `torch.cuda.empty_cache()`.
3. Load the DiT (int8 dynamic) + small VAE, assemble the pipeline.
4. Call `pipe(prompt_embeds=..., prompt_embeds_mask=..., ...)` — the `__call__`
   accepts precomputed embeds, so it never touches the encoder again.
5. DiT runs on **CPU** (7 GB int8 exceeds 6 GB VRAM); VAE small, on GPU.

**Quant:** torchao `Int8DynamicActivationInt8WeightConfig` on the DiT and
`Int8WeightOnlyConfig` on the text encoder. `quantize_(model, cfg)`.

### Supported aspect ratios (for the encode-first script)

From the model page Quick Start — scale width/height together to one of these:

```python
aspect_ratios = {
    "1:1":  (2048, 2048),
    "4:3":  (2400, 1792),
    "3:4":  (1792, 2400),
    "3:2":  (2528, 1696),
    "2:3":  (1696, 2528),
    "16:9": (2752, 1536),
    "9:16": (1536, 2752),
}
```

### Native RGBA (transparent) generation — the icon/schematic payoff

Recommended prompt format for transparency (this is the advantage over the
multi-file Flux-2-Klein restore for icon/schematic work):

```python
image = pipe(
    prompt="This is an RGBA image with transparency. A cute cartoon dragon sticker. "
           "The image has alpha channel and the background is transparent.",
    width=2048, height=2048,
    num_inference_steps=40,
    generator=torch.Generator("cuda").manual_seed(42),
).images[0]
image.save("transparent_example.png")
```

### ⚠️ Tokenizer gotcha (blocks a naive offline load)

`from_pretrained` works **offline** (`HF_HUB_OFFLINE=1` proves the cache is used
implicitly), but `AutoTokenizer.from_pretrained("Qwen/Qwen-Image-2.1")` fails
offline because the tokenizer files live in the
`processor/` subdir, not the repo root. Load the tokenizer from the snapshot's
`processor/` dir (all files present there) or point AutoTokenizer at that
subdir. The text encoder itself loads fine from the repo root via accelerate.

### Download method that works (do NOT use `curl`/`dl.sh` anymore)

The old `dl.sh` used `curl` and worried about the `models--Qwen/Qwen-Image-2.1`
(single-slash) vs `models--Qwen--Qwen-Image-2.1` (canonical) bug. It did **not**
happen — the download landed in the canonical tree. Don't rebuild `dl.sh`.

Use the `hf` CLI instead, with XET disabled (XET stalls at 0 MB/s on this box):

```bash
HF_TOKEN="$HF_TOKEN" HF_HUB_DISABLE_XET=1 hf download \
  Qwen/Qwen-Image-2.1 --type model --max-workers 8
```

Resumes `.incomplete` blobs automatically; plain HTTPS is ~3–9 MB/s.

`~/.cache/huggingface/hub` is a **FUSE mount** (`/dev/sda1`, user_id=0,
allow_other): files show `root:root` and can't be chowned, but they're
writable (930 GB free). The parent `~/.cache/huggingface` was `root:755`
and blocked hf's runtime once — fixed with
`sudo -n chown -R jan:jan ~/.cache/huggingface` (chown is NOPASSWD in
`/etc/sudoers.d/chezmoi-pi`).

### Env (venv `~/.venvs/qwenimg`; activate `source ~/.venvs/qwenimg/bin/activate`)

- torch 2.14.0+cu130 (verified `cuda.is_available()` + on-gpu matmul on sm_75),
  torchvision 0.29, transformers 5.17.0, torchao 0.18.0, accelerate 1.15.0,
  diffusers 0.41.0.dev0 (from git main — needed for `QwenImage21Pipeline`).
- pip installs: `uv pip install --python ~/.venvs/qwenimg/bin/python -i https://download.pytorch.org/whl/cu128 --extra-index-url https://pypi.org/simple`.
  (Note: index is `cu128` while the installed torch tag is `cu130` — verify
  this is intended when reproducing.)
- `hf` = the `huggingface_hub` package CLI (renamed from `huggingface-cli`).

### Pipeline API

(`true_cfg_scale` defaults to 4.0.) The HF blog example uses
`DiffusionPipeline.from_pretrained(..., device_map="cuda")` — **that will OOM
here**; it assumes an 80 GB GPU.

```python
from diffusers import QwenImage21Pipeline
pipe = QwenImage21Pipeline.from_pretrained("Qwen/Qwen-Image-2.1", torch_dtype=torch.bfloat16).to("cuda")
image = pipe(prompt=..., num_inference_steps=40, true_cfg_scale=4.0, generator=torch.Generator("cuda").manual_seed(42)).images[0]
```

### Caveats

Lossy int8 + CPU-DiT smoke test — expect softer detail than bf16/FP8.
`.incomplete` blobs resume on re-run.

---

## Why it matters here

Single-model install + diffusers-native fits the existing `comfy-cli`/restore
workflow and would replace the multi-file Flux-2-Klein restore above. The RGBA
output is a real advantage for icon/schematic generation. **Prompt-rewriting
checkpoints** (`Qwen/Qwen-Image-2.1-PE-T2I`, `-PE-I2I`, Qwen3.5-VL 9B) improve
results.

> **License caveat (read before generating anything for a product):** Qwen
> Research License = **non-commercial use only**; commercial use needs a
> separate license from the author. See `discoveries/ai-rehab-handout.md`
> for how this bites the rehab handout (Q3/14).

## Notes / open items

- **DONE-HERE, CONTINUE ELSEWHERE.** The final generate call will **not** run on
  this box (6 GB VRAM + a faulting external NTFS drive). See
  [`handover-continue-on-bigger-box.md`](handover-continue-on-bigger-box.md):
  finish on a machine with ≥16 GB VRAM using the simple INT8 GPU path (skip the
  encode-first gymnastics).
- **Download is complete** (see the status section above). On the next machine,
  re-download fresh (`hf download unsloth/Qwen-Image-2.1-FP8`) — the int8
  recommendation above holds the two components at once under 15 GB.
- **Local smoke-test bug (fixed).** The local `generate.py` died with
  `NameError: name '_PIPELINE' is not defined` at the embed step — `_PIPELINE`
  was a local in `main()`, not visible to `build_embed_pipeline`. Fixed by
  passing the class in. Only affected the local encode-first attempt; the simple
  INT8 GPU path on the next machine is unaffected.
- Qwen-Image-2.1 is a lighter, RGBA-capable alternative that would replace the
  multi-file Flux-2-Klein restore. The single-model diffusers install is now
  proven end-to-end except for the actual generate call.
- Consider adding back a `--purge`-safe guard: if no NVIDIA GPU is present,
  this flow never touches anything, but the broader AI tooling should not
  install `nvidia-cuda-toolkit` either (see the `lspci` pattern used in
  `.chezmoiscripts/run_once_3_devtools_3podman_nvidia_cdi.sh`).
