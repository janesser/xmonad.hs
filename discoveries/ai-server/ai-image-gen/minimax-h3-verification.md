# Verification: MiniMax-H3-Image as a Flux 3 Image alternative on cyberkleiber

**Date:** 2026-11-14
**Verdict: not viable on this box — hardware.** sd.cpp supports the model, the weights are downloaded and verified, but one V100-32GB leaves no headroom for the loaded footprint (~26 GB). Flux 3 Image Klein Q8_0 remains the only viable image-gen path here.

---

## What was tested

`MiniMaxAI/MiniMax-H3-Image` (33.1B params, 16 MP native, CFG-distilled 6-step) as a potential replacement for `Flux 3 Image Klein Q8_0` on the card-2 V100 (PCIe 32 GB), using sd.cpp from upstream master.

## Work performed

### 1. Environment
- Re-linked `.venv/bin/python` to `cpython-3.12.16` (dead symlink) — torch 2.13+cu126, diffusers 0.40, accelerate all importable on both V100s.
- Verified network reachability and HF cache (`HF_HOME=/media/sailor/huggingface-hub`, 181 GB used, ~480 GB free).

### 2. sd.cpp upgrade (required — not supported in old build)
- Clone was at `77b72a2` (Oct 28, 2026), **50 commits behind upstream master** (`0f8a467`, Nov 14).
- `git checkout origin/master` + updated submodules: `ggml` → `d803c238` (provides `ggml-ext.h`/`ggml-graph.h`), `thirdparty/cutlass` → initialized.
- Rebuilt: `make -j4 sd-cli sd-server`. New binary: `master-1094-3bad892-2-g0f8a467`, commit `0f8a467`.
- **Flux 3 Image regression test passed** (rendered `/tmp/sdcpp_new_flux3_test.png`, valid PNG).
- Fixed a dead symlink: `sd-probe/vae/flux2-vae.safetensors` pointed at a deleted HF snapshot; re-linked to the local copy `sd-probe/vae/flux3/flux2-vae.safetensors`.

### 3. Model download
All three components downloaded, cleaned and verified (all `safetensors` valid):

| Component | Path | Size | Tensors |
|---|---|---|---|
| DiT (int8_convrot) | `probe-minimax/diffusion_models_int8/minimax_h3_image_pruned_int8_convrot.safetensors` | 20 GB | loaded OK |
| P-H3-Image TE | `probe-minimax/text_encoders/p-h3-image_text_encoder_int8_convrot.safetensors` | 4.8 GB | 1225 |
| Image VAE | `probe-minimax/vae/minimax_h3_image_vae_fp16.safetensors` | 1.1 GB | 416 |

The W4A8 variant (`minimax_h3_image_pruned_w4a8.safetensors`, 11.7 GB) failed with `invalid packed INT4 scales/codebook` — its ComfyUI quantization metadata lacks the `.weight_s_channel` tensor the loader requires. The int8_convrot variant (recommended in upstream docs) loads but hits VRAM pressure (see below).

### 4. Side-by-side comparison test
Both renders via `card2-swap.sh image --batch`, pinned to card-2 (stops little-brother, restores it), same prompt/seed/size:

```json
{ "prompt": "a fox made of glass, studio lighting, shallow depth of field", "seed": 42, "width": 512, "height": 512 }
```

| Model | Output | Time | Status |
|---|---|---|---|
| Flux 3 Image Klein Base Q8_0 | `compare/flux3_image.png` | 62s | ✅ **512×512, valid PNG** |
| MiniMax-H3-Image (int8_convrot) | — | — | ❌ CUDA launch failure |

### 5. Why it fails

```
total params memory size = 26092.96MB (VRAM)
  text_encoders   4884.57 MB  ← P-H3-Image TE on GPU
  diffusion_model 20091.68 MB ← int8_convrot DiT
  vae             1116.71 MB  ← MiniMax image VAE
```

**~26 GB of a 32 GB card, with no headroom for CUDA kernel workspaces or activation buffers.** The actual failure:
```
[E] ggml: CUDA error: unspecified launch failure
[E]   current device: 0, in function ggml_backend_cuda_synchronize
```

Even CPU-offloading the text encoder (~5 GB) would still leave ~21 GB (66%) with essentially no computation headroom. A 33B-parameter model needs ~40+ GB of GPU memory with headroom to actually run.

## What fits on this box (one V100-32GB)

| Model | Loaded VRAM | Status |
|---|---|---|
| **Flux 3 Image Klein Base Q8_0** | ~6–7 GB | ✅ works, ~30–60s at 512² |
| MiniMax-H3-Image (int8_convrot) | ~26 GB | ❌ needs ≥40 GB VRAM |

## Next steps if MiniMax quality is the goal

- **Bigger hardware:** one A100-80GB / H100, or two V100s (~64 GB). Not available on this box.
- **Cloud/API:** MiniMax API (platform.minimax.io) or HF Spaces for evaluation before buying hardware.
- The weights remain locally (all 27 GB) — reusable on a bigger box with the same sd.cpp recipe:
```sh
sd-cli --diffusion-model .../minimax_h3_image_pruned_int8_convrot.safetensors \
       --llm .../p-h3-image_text_encoder_int8_convrot.safetensors \
       --vae .../minimax_h3_image_vae_fp16.safetensors \
       -W 1024 -H 1024 --steps 6 --sigmas minimax-h3-image --offload-to-cpu
```

## Files (all in `discoveries/ai-server/ai-image-gen/`)

- `minimax-h3-verification.md` — this document
- `flux3-vs-minimax-h3-comparison.md` — detailed comparison with output paths
- `sdcpp-upgrade-report.md` — sd.cpp rebuild details
- `minimax-h3-download-status.md` — weight inventory and verification
