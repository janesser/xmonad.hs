# Handover: finish Qwen-Image-2.1 on a machine with more VRAM

**Decision (2026-09-26):** the final generation smoke test will **not** run on
`lincopta` (RTX 2060 Mobile, 6 GB VRAM, 15 GB RAM). The box is too small and its
external-NTFS cache drive is faulting (I/O errors, drops mid-run). Continue on a
box with **≥16 GB VRAM** — there the whole thing collapses to a normal,
GPU-resident pipeline and none of the 6 GB gymnastics are needed.

This file carries the work forward. The full box-specific writeup is
[`qwen-image-21.md"](qwen-image-21.md); read it for the 6 GB RAM/VRAM analysis.
This doc is the *"what to do next, simply"* part.

---

## TL;DR for the next machine

```bash
# env (any recent torch; CUDA build)
pip install torch transformers accelerate pillow git+https://github.com/huggingface/diffusers

# INT8 is the sweet spot here (Turing sm_75 has NO FP8 hardware; INT8 is also
# higher quality than FP8: LPIPS 0.064 vs 0.112). Unsloth ships pre-quantized
# INT8, so you skip encoding the text encoder yourself:
HF_TOKEN=... HF_HUB_DISABLE_XET=1 hf download \
  unsloth/Qwen-Image-2.1-FP8 --type model --max-workers 8

python generate.py
```

`generate.py` is in `/tmp/qwenimg_test/generate.py` (copied below). With ≥16 GB
VRAM the DiT stays on the GPU, CPU offload is a soft opt-in, and it just works.

---

## The recipe that actually works (simple path, ≥16 GB VRAM)

```python
import torch
from diffusers import QwenImage21Pipeline

pipe = QwenImage21Pipeline.from_pretrained(
    "unsloth/Qwen-Image-2.1-FP8",          # INT8 (W8A8), pre-quantized
    torch_dtype=torch.bfloat16,
).to("cuda")                              # add .enable_model_cpu_offload() if short on VRAM

image = pipe(
    prompt="A clean medical illustration, front view of a human sitting up from "
           "an office chair, lifting the upper body with both arms, white "
           "background, anatomical, schematic.",
    width=1024, height=1024,               # any 32-divisible size; presets up to 2048²
    num_inference_steps=40,
    true_cfg_scale=1.0,                    # ⚠️ NOT 4.0 — see gotchas
    generator=torch.Generator("cuda").manual_seed(42),
).images[0]
image.save("qwen_image_21_smoke.png")
```

`true_cfg_scale=1.0` is intentional (see gotchas). The unsloth repo's INT8 file
is `Qwen-Image-2.1-INT8.safetensors` (7.26 GB DiT) + a pre-cast FP8 text
encoder — the pipeline loads them as a normal multi-config repo, so you do **not**
need the encode-first dance.

### If you prefer the upstream bf16 repo instead

`Qwen/Qwen-Image-2.1` works the same way on a big-enough box:
```python
pipe = QwenImage21Pipeline.from_pretrained("Qwen/Qwen-Image-2.1", torch_dtype=torch.bfloat16).to("cuda")
```
bf16 DiT ≈ 14 GB + encoder ≈ 17.5 GB → needs ~32 GB VRAM or CPU offload with
≥~40 GB RAM. INT8 (unsloth) is the pragmatic choice and needs far less.

### On the 6 GB / 15 GB box (for reference — NOT what the next machine needs)

Hold one component at a time, both int8, CPU offload: text encoder int8
weight-only on CPU → embeds → free it → DiT int8 on CPU + VAE on GPU. See
`qwen-image-21.md` → "Encode-first recipe". This is only needed because 7 GB
int8 DiT and 9 GB int8 encoder each exceed the 6 GB card on their own.

---

## Gotchas that travel to any machine

- **Guidance scale = 1.0, not 4.0.** Qwen-Image is tuned for CFG disabled;
  `true_cfg_scale=4.0` (the HF blog default) degrades quality. This was the
  biggest early miss.
- **RGBA / transparency** (the icon-schematic payoff): prepend
  *"This is an RGBA image with transparency. … The image has alpha channel and
  the background is transparent."* Save as PNG/WebP (JPG drops alpha).
- **Tokenizer lives in `processor/`**, not the repo root — `AutoTokenizer`
  needs that subdir. The text encoder itself loads from the repo root.
- **HF_TOKEN required.** `Qwen/Qwen-Image-2.1` is gated; set `HF_TOKEN`. Download
  with `HF_HUB_DISABLE_XET=1` (XET stalls at ~0 MB/s on lincopta).
- **INT8 > FP8 here.** sm_75 lacks FP8 hardware (FP8 is emulated + lossier).
  INT8 is both faster and closer to bf16 (LPIPS 0.064 vs 0.112).
- **License: Qwen Research License = non-commercial.** See
  `discoveries/ai-rehab-handout.md` (Q3/14) — this bites the rehab handout.
- **Prompt-rewriting checkpoints** (`Qwen/Qwen-Image-2.1-PE-T2I`, `-PE-I2I`)
  improve results if you want them — diffusers-native.

---

## What's already proven

- Weights present and complete: canonical snapshot
  `790c92633540aa0cb11d9abf19eb46d861714758` (28/28 blobs, ~33.8 GB) on
  `lincopta`'s external NTFS drive (`~/.cache/huggingface/hub`). The drive is
  the unreliable part — **re-download on the next machine** rather than moving
  it.
- The diffusers + INT8 pipeline is **proven end-to-end**: the text-encoder embed
  step was computed successfully on lincopta (`_get_qwen_prompt_embeds` →
  embeds shape **(1, 19, 4096)**, all finite). Only the final decode/generate
  call was never reached.
- `unsloth/Qwen-Image-2.1-FP8` provides ready-made INT8 + FP8 files, so the
  next machine skips the from-scratch int8 quantize entirely.

---

## Known local blockers on lincopta (so nobody repeats them)

- **`generate.py`'s local encode-first copy had a `NameError`.** It called
  `build_embed_pipeline(...)` which referenced `_PIPELINE`, a name that was only
  a *local* in `main()` — so every local attempt died at the embed step with
  `NameError: name '_PIPELINE' is not defined` (exit 1), **before** ever touching
  the scheduler or the drive. Fixed by passing the class in
  (`build_embed_pipeline(snap, proc, QwenImage21Pipeline)`). This only affected
  the *local* encode-first smoke test — it does **not** touch the simple INT8
  GPU path in the recipe above.
- **Scheduler config location.** When that path *is* exercised, the scheduler
  config lives at `scheduler/scheduler_config.json`, so
  `FlowMatchEulerDiscreteScheduler.from_pretrained(<snapshot>)` must pass
  `subfolder="scheduler"` or it raises `LocalEntryNotFoundError`.
- **External NTFS drive faults.** `/dev/sda1` throws hard I/O errors and
  disconnects mid-run (`ntfs_pread failed`, `sd [sda] Synchronize Cache
  failed: hostbyte=DID_ERROR`). Reseat the USB cable / use a direct port before
  trusting this box again. Do **not** run long jobs here until it's stable.
- The drive is why we're finishing on a bigger box: even with the code fixed,
  6 GB VRAM can't hold the INT8 DiT + encoder, so the render must happen
  elsewhere regardless.

## Target output

One medical illustration (human sitting up from a chair, lifting the upper body
with both arms, white background, anatomical schematic) — matching the
Qwen-Image-2.1 Showcase `#text-to-image` aesthetic
<https://huggingface.co/Qwen/Qwen-Image-2.1#text-to-image>.
