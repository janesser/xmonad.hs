# AI image generation (NVIDIA)

Handover notes for local image-generation backends on this box.

- **[qwen-image-21.md](qwen-image-21.md)** — *current direction.* Qwen-Image-2.1,
  7B unified text-to-image + edit model with native RGBA transparency. Weights
  downloaded, env ready; a CPU/int8 generation smoke test is still pending under
  the 6 GB VRAM / 15 GB RAM constraint.
- **[comfy-flux-2-klein.md](comfy-flux-2-klein.md)** — the *older* ComfyUI /
  multi-file Flux-2-Klein restore. Superseded by Qwen-Image-2.1; kept as
  historical handover.

**Hardware context:** RTX 2060 Mobile (6 GB VRAM, Turing sm_75, no FP8), 15 GB
RAM. The quantized/CPU encode-first path exists only because of this.

See `.chezmoiscripts/run_once_5_aitools_*` for the related AI backend setup
(llama.cpp, btop). All image flows are gated behind an NVIDIA-presence check.
