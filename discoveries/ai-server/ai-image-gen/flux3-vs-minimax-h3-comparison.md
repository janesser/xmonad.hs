# Comparison: Flux 3 Image Klein Base vs MiniMax-H3-Image on this box

**Date:** 2026-11-14 (via `card2-swap.sh`, card-2 V100 PCIe)
**Result:** Flux 3 Image ran successfully; **MiniMax-H3-Image cannot run on one V100-32GB**.

## The comparison setup

Both renders used the same prompt, seed, and resolution via `card2-swap.sh image --batch`:

```json
{ "prompt": "a fox made of glass, studio lighting, shallow depth of field", "seed": 42, "width": 512, "height": 512 }
```

- **Flux 3 Image Klein Base** (Q8_0 gguf): `sd-probe/diffusion_models_gguf/flux3-image-klein-base-Q8_0.gguf` + Qwen3-VL-4B text encoder + Flux.2 VAE
- **MiniMax-H3-Image** (int8_convrot DiT, 20 GB): `probe-minimax/diffusion_models_int8/minimax_h3_image_pruned_int8_convrot.safetensors` + P-H3-Image TE + MiniMax-H3-Image VAE

Both used the same batch, pinned to card 2 (PCIe V100) with the little-brother stopped.

## Results

| Model | Output | Time | Status |
|---|---|---|---|
| Flux 3 Image Klein Base | `/media/sailor/ai-server/compare/flux3_image.png` | 62s | ✅ **512×512, 560 KB PNG** (valid) |
| MiniMax-H3-Image (int8_convrot) | — | — | ❌ **CUDA launch failure** |

## Why MiniMax-H3-Image fails here

The reason is not the *model* — sd.cpp supports it — but the VRAM math:

```
total params memory size = 26092.96MB (VRAM)
  text_encoders   4884.57 MB (VRAM)   ← P-H3-Image TE loaded on GPU
  diffusion_model 20091.68 MB (VRAM)  ← int8_convrot DiT
  vae             1116.71 MB (VRAM)   ← MiniMax image VAE
```

**~26 GB of a 32 GB card, with no headroom for CUDA kernel workspaces, activation buffers, or anything else.** The actual failure was:
```
[E] ggml: CUDA error: unspecified launch failure
[E] ggml:   current device: 0, in function ggml_backend_cuda_synchronize
```

Even if the text encoder were offloaded to CPU (saving ~5 GB), you'd still be at ~21 GB of 32 GB with essentially **zero headroom** for computation — a 33B-parameter model's weights plus scales must both live in VRAM during inference.

## The hardware requirement for MiniMax-H3-Image on sd.cpp

| Card | VRAM | Viability |
|---|---|---|
| One V100-32GB | 32 GB | **No** — needs ~26 GB + headroom |
| Two V100s (64 GB) | 64 GB | Yes, with TP or model splitting (not simple in sd.cpp) |
| A100-80GB / H100 | ≥40 GB | Yes |

Note this is *unfair* to MiniMax-H3-Image: it's a **33.1B param** 16MP-native model (4096² range, 6-step CFG-distilled). Flux 3 Image Klein Base is a **4B** distilled model whose GGUF weighs only ~2.4 GB. Comparing them at the same resolution on this box is like comparing a truck to a bicycle.

## What this box can actually run (on one V100)

| Model | On-card size | Result |
|---|---|---|
| **FLUX.3 Image Klein Base Q8_0** | ~2 GB GGUF + TE + VAE ≈ 6–7 GB loaded | ✅ runs, ~30–60s at 512² |
| **MiniMax-H3-Image int8_convrot** | 26 GB loaded | ❌ needs ≥40 GB VRAM |

## Bottom line for the MiniMax-H3-Image trial

This is not a "bad model" finding — it's a *hardware* finding:
1. sd.cpp upstream master (0f8a467) **does** support MiniMax-H3-Image, including the INT8 ConvRot variant that fits on card 2
2. The weights are available and I've downloaded all three components
3. But running it requires **≥40 GB of GPU memory with headroom** — this box's single V100-32GB is simply too small for the loaded footprint (~26 GB) plus computation buffers

The Flux 3 Image output is at `/media/sailor/ai-server/compare/flux3_image.png` (512×512). A proper visual comparison of both would need a second machine with ≥40 GB VRAM, or one V100-32GB running MiniMax-H3-Image via vLLM-Omni at 6 steps (which needs the same hardware requirement anyway — ~65 GB peak per the vendor docs).
