# AI image generation (NVIDIA)

Handover notes for local image-generation backends on this box.

- **[stable-diffusion-cpp.md](stable-diffusion-cpp.md)** — *current direction.*
  sd.cpp + GGUF is now the preferred image-gen path (adopted 2026-10-08). Loads
  a pre-quantized GGUF straight to the card, sidestepping the bf16 load
  transient that sank the vLLM-Omni probe. Ranked model range (SDXL, FLUX.1-dev,
  FLUX.2-klein, Qwen-Image-2512) with fit + the black-image quant gotcha, for
  both the V100 (32 GB) and the RTX 2060 Mobile (6 GB).
- **[qwen-image-21.md](qwen-image-21.md)** — Qwen-Image-2.1, 7B unified
  text-to-image + edit with native RGBA transparency. Weights downloaded; the
  final render runs on a ≥16 GB box, not this 6 GB one. See
  `handover-continue-on-bigger-box.md` for the simple ≥16 GB recipe.
- **[vllm-omni-2512-probe.md](vllm-omni-2512-probe.md)** — the abandoned
  vLLM-Omni + Qwen-Image-2512 probe: why it failed on fit (~57 GB bf16 transient
  vs 32 GB) and a clean-up checklist for later.
- **[comfy-flux-2-klein.md](comfy-flux-2-klein.md)** — the *older* ComfyUI /
  multi-file Flux-2-Klein restore. Superseded by Qwen-Image-2.1; kept as
  historical handover.

**Bridges covered:** this thread now spans two boxes — the **RTX 2060 Mobile
(6 GB, Turing sm_75, no FP8)** from `qwen-image-21.md`, and the **V100 (32 GB,
cyberkleiber)** the heavy 1024² models (FLUX, Qwen-Image-2512) actually run on.
On the 6 GB box the quantized/CPU encode-first path exists only because of the
VRAM/RAM squeeze; on the V100 sd.cpp GGUF fits without gymnastics.

See `.chezmoiscripts/run_once_5_aitools_*` for the related AI backend setup
(llama.cpp, btop). All image flows are gated behind an NVIDIA-presence check.
