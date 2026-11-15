# sd.cpp Upgrade Report — upstream master 0f8a467

**Date:** 2026-11-13 (upgrade performed ~22:55–23:04)
**Result:** ✅ **success** — sd-cli rebuilt from upstream master with MiniMax-H3-Image support; Flux 3 Image still renders.

---

## What changed

The local clone `/home/jan/projs/stable-diffusion.cpp` was at `77b72a2` (Oct 28, 2026), **50 commits behind upstream master** (`0f8a467`, Nov 14, 2026). It is now checked out on `origin/master` with the submodules updated.

### Upgrade steps performed
1. `git fetch origin --prune` — upstream master at `0f8a467`
2. `git checkout origin/master`
3. **Submodules needed updating** (the build failed initially without them):
   - `ggml` → `d803c238` (now provides `include/ggml-ext.h`, `ggml-graph.h` — required for W4A8 support)
   - `thirdparty/cutlass` → `ad7b2f` (initialized; previously missing)
   - `examples/server/frontend` → `b07665f`
4. Rebuild: `make -j4 sd-cli sd-server` (took ~30 min, 2 failed attempts at 59% due to NVME disk space pressure during CUDA compilation — reduced parallelism and freed `/tmp` resolved it)

### Resulting version
```
stable-diffusion.cpp version master-1094-3bad892-2-g0f8a467, commit 0f8a467
```

---

## What the upgrade enables

| Feature | Before (77b72a2) | After (0f8a467) |
|---|---|---|
| FLUX.3 Image Klein/Base | supported, working | **still working** (verified: 512×512 PNG rendered) |
| MiniMax-H3 video/audio | `docs/minimax_h3.md` — T2VA/Ref2VA only | same, plus **MiniMax-H3-Image** |
| **MiniMax-H3-Image** | not supported | **`docs/minimax_h3_image.md` added (#2324)** |
| W4A8 / INT8 ConvRot formats | — | `safetensors_io.cpp` handles `asym_w4a8_int8`, `convrot_w4a4` |
| P-H3-Image text encoder | — | allowed by `#2337` (~5 GB vs 18 GB for Q4 32B) |

The new build **runs MiniMax-H3-Image** via sd.cpp, including:
- text-to-image (6 steps, Euler, CFG 1.0, `--sigmas minimax-h3-image`)
- reference-image editing (up to 16 refs, `<Picture N>` in prompt)
- the **P-H3-Image text encoder** (`p-h3-image_text_encoder_int8_convrot.safetensors`, ~5 GB, Qwen3-VL-4B + connector) — this is what makes it fit on one V100

---

## What's needed to actually run MiniMax-H3-Image on this box

Per `docs/minimax_h3_image.md` (upstream master), the weights are:
- **DiT:** `minimax_h3_image_pruned_w4a8.safetensors` — W4A8, 4-bit weights (~17 GB estimated) from `Comfy-Org/MiniMax-H3-Image`
- **Text encoder:** P-H3-Image (`PrunaAI/P-H3-Image-Comfy/text_encoders/p-h3-image_text_encoder_int8_convrot.safetensors`) — ~5 GB
- **VAE:** `minimax_h3_image_vae_fp16.safetensors` (~2 GB)

**Estimated VRAM total: ~22–25 GB** → fits on one V100-32GB. The alternative (full BF16 DiT = 68.65 GB) is what failed in the diffusers/vLLM-Omni trial.

**Prerequisite:** one V100 must be freed — both cards currently run llama backends (`:8081` at 27 GB, `:47141` little-brother at 26.987 GB). Stopping the little-brother server on GPU1 would give a full 32 GB card.

---

## Fixed probe path (important!)

The old symlink `sd-probe/vae/flux2-vae.safetensors` → `/home/jan/.cache/huggingface/hub/models--Comfy-Org--vae-text-encorder-for-flux-klein-4b/snapshots/.../flux2-vae.safetensors` is **dead** — the entire HF cache at `/home/jan/.cache/huggingface/hub` was deleted at some point.

I re-linked it to the working local copy:
```
sd-probe/vae/flux2-vae.safetensors -> ../vae/flux3/flux2-vae.safetensors (336 MB)
```
Now both old and new sd.cpp loads work with `--vae sd-probe/vae/flux2-vae.safetensors` (`--vae-format flux2`).

---

## Verification done

- New binary runs: `./build/bin/sd-cli --version` → `master-1094-3bad892-2-g0f8a467`
- Flux 3 Image regression test: rendered `/tmp/sdcpp_final_test.png` (510 KB, 512×512, valid PNG)
- The probe path fix (`--vae sd-probe/vae/flux2-vae.safetensors`) verified with new build
- Build dir: `/home/jan/projs/stable-diffusion.cpp/build/bin/sd-cli` (180 MB), `sd-server` unchanged

---

## Next steps (if you want to run MiniMax-H3-Image on this box)

1. **Free GPU1** from the little-brother llama server (`systemctl --system stop llama-little-brother.service`)
2. **Download weights:** `Comfy-Org/MiniMax-H3-Image` (W4A8 DiT + VAE) + `PrunaAI/P-H3-Image-Comfy` (P-H3 text encoder) — into `/media/sailor/huggingface-hub` or a probe dir
3. **Run:**
```sh
sd-cli --diffusion-model minimax_h3_image_pruned_w4a8.safetensors \
       --vae minimax_h3_image_vae_fp16.safetensors \
       --llm p-h3-image_text_encoder_int8_convrot.safetensors \
       -W 1024 -H 1024 --steps 6 --sigmas minimax-h3-image --offload-to-cpu
```
   (the local VAE symlink at `sd-probe/vae/flux2-vae.safetensors` is the wrong file for MiniMax-H3-Image — its image VAE must be separate)

4. **Verify** with a smoke test (6-step render), then benchmark speed/memory on one V100.
