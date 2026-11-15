# Trial: MiniMax-H3-Image on the AI server

**Verdict: not viable on this box.** The model's DiT (68.65 GB BF16) exceeds the combined VRAM of both V100s (2 × 32 GB = 64 GB), and every offload strategy documented by MiniMax/vLLM-Omni needs ~200 GiB host RAM — this box has ~10 GiB available.

Date: 2026-11-13. Trial run on `cyberkleiber`.

---

## What was tested

`MiniMaxAI/MiniMax-H3-Image` (open-weight, ~16 MP text-to-image + editing; DiT = 33.1B params, BF16) as a replacement for the current flux3/sd.cpp path on `/media/sailor/ai-server/`.

## Environment set up

- Re-linked `.venv/bin/python` (dead symlink to removed `cpython-3.12`) → `cpython-3.12.16`: torch 2.13.0+cu126, diffusers 0.40.0, accelerate 1.12.0, huggingface_hub 1.33.0 all importable and CUDA-visible on both V100s.
- `diffusers 0.43.0` (downloaded to `/tmp/h3_trial/pkgcheck`) confirmed to ship the required pieces: `MiniMaxH3ImageModularPipeline`, `transformer_minimax_h3_image.py`, `autoencoder_kl_minimax_h3_image.py`. The venv's 0.40.0 does **not**.
- HF cache: `HF_HOME=/media/sailor/huggingface-hub` (181 GB used, ~480 GB free); weights download started in the background (`/tmp/h3_trial/dl.log`).
- Network confirmed reachable (huggingface.co → 200).

## Hardware state (measured 2026-11-13)

| Resource | Value |
|---|---|
| GPU 0 | Tesla V100-SXM2-32GB, **27000 MiB used** by `llama serve` on :8081 |
| GPU 1 | Tesla V100-PCIE-32GB, **26987 MiB used** by little-brother llama (:47141) |
| Total VRAM | 64 GB (both fully occupied) |
| System RAM | 31.3 GB total, **~8.7 GB free** / ~10.3 GiB available |

## Why it fails (three independent proofs)

**1. VRAM arithmetic (the DiT alone exceeds the box).**
The HF model card lists the transformer as 68.65 GB BF16 ([model zoo](https://huggingface.co/MiniMaxAI/MiniMax-H3-Image#model-zoo)). Two V100s give 64 GB. Even with perfect TP2 splitting (one shard per GPU), each rank holds ~34.3 GB — over the 32 GB per-card limit on both cards.

**2. The model's own offload requirement.**
MiniMax's docs state: "The transformer and the text encoder are about 62 GB each in bfloat16 and do not fit on an 80 GB GPU together, so the examples use auto offloading… A 4736x3552 image then peaks at about 65 GB" — i.e. even with per-component auto-offloading, ~65 GB of GPU memory is needed. The vLLM-Omni recipes (installed at `/media/sailor/ai-server/vllm-omni`) validate MiniMax-H3 on **2 × RTX 5090 (2 × 32 GB)** with `--dlo-resident-layers 20` as the **absolute floor** — and require **200 GiB system RAM** for the DLO offload machinery, with 384 GiB recommended. This box: ~10 GiB.

**3. The text encoder adds a second load.**
The DiT's text encoder lives in `MiniMaxAI/MiniMax-H3/text_encoder` (66.7 GB BF16). With ComponentsManager auto-offload the components stream sequentially, so peak is one component's ~65 GB — but accelerating a 68 GB DiT across two 32 GB cards still fails at ~34 GB/rank (proof #1), and any vLLM-Omni DLO profile needs the host-RAM budget above.

## What would run here

The current path is the only one that fits, documented in `ai-image-gen/stable-diffusion-cpp.md` and driven by `switch-ai-backend.sh image`:

| Model | On-box weights | VRAM needed | Status |
|---|---|---|---|
| **FLUX.2-klein Q4_0** (sd.cpp) | `/media/sailor/ai-server/sd-probe/diffusion_models/flux-2-klein-4b-Q4_0.gguf` + encoder + VAE (~6.5 GB total) | ~13 GB, fits one V100 | works |
| **FLUX.3-image-klein Q8_0** | `sd-probe/diffusion_models_gguf/flux3-image-klein-base-Q8_0.gguf` (6.1 GB) | ~20 GB, fits one V100 | works |
| MiniMax-H3-Image | 68.65 GB DiT + 66.7 GB TE = ~135 GB | 64 GB VRAM + ~200 GiB RAM | **no** |

## Verdict and next steps

MiniMax-H3-Image needs an 80+ GB GPU (A100/H100/RTX 5090-class) per the vendor's own documentation, or at least two 32 GB cards with ~200 GiB host RAM for the DLO offload — this box satisfies neither. It cannot replace the flux3/sd.cpp path here.

If image quality beyond FLUX.2-klein is the goal:
1. **Stay on-box:** `sd-probe/diffusion_models_gguf/flux3-image-klein-base-Q8_0.gguf` (the newer Flux 3 variant, 6 GB Q8_0, already present) runs on sd.cpp via the same switch-ai-backend.sh machinery.
2. **New hardware:** one 2×A100-80GB or 2×RTX-5090 box + 200+ GiB RAM would run MiniMax-H3-Image at vLLM-Omni (`vllm serve MiniMaxAI/MiniMax-H3-Image --omni`), ~6 steps, 16 MP.
3. **Cloud/API:** the MiniMax API or HF Spaces for evaluation before buying hardware.

A background weight download is running in `/tmp/h3_trial/dl.log` (slow — ~190 KB/s network) as a record of connectivity/availability; it will not be completed here. The model is accessible from this box; only the compute fails.
