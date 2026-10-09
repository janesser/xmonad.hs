# vLLM-Omni + Qwen-Image-2512 image-gen probe — WRAP-UP (abandoned 2026-10-08)

Status: **probe ran, failed on fit; the vLLM-Omni *image-gen* path is
abandoned in favour of stable-diffusion.cpp** (see `stable-diffusion-cpp.md`).
This doc records *why* it failed and leaves a **clean-up checklist** for later.

> Scope note: this is **image-gen only**. vLLM-Omni remains the box's omni
> **text/audio** engine (§11 of `ai-server.md`, Qwen2.5-Omni). We are not
> tearing down vLLM-Omni — only the image-gen 2512 probe and its artifacts.

---

## What was tried

`probe1b_generate.fixed.sh` borrowed the single V100 (killed the ornith reaper
tree — see below), then launched
`vllm-omni/examples/offline_inference/text_to_image/text_to_image.py` with
`--model Qwen/Qwen-Image-2512 --quantization int8 --height 1024 --width 1024
--num-inference-steps 50` toward `probe_2512.png`, under `timeout 1800`.

## What we learned (the real root cause)

The 30-min stall was **not** a model bug — it was a **VRAM-contention
artifact**: ornith (the `--models-max 1` llama.cpp backend) held all 32 GB, so
the vLLM process never got GPU and appeared "stuck". Once the GPU was freed the
stall moved to the *actual* blocker:

- **Qwen-Image-2512 is a 20B MMDiT: transformer bf16 = 40.9 GB (9 shards) +
  Qwen2.5-VL text encoder bf16 = 16.6 GB → ~57.5 GB bf16 total.** It does **not**
  fit the V100's 32 GB.
- vLLM-Omni's `--quantization int8` **loads bf16 into RAM/VRAM then quantizes on
  the fly**, so even an "int8" run touches ~57 GB transient → **OOM at load**.
  (The only clean-run evidence is absent — every prior "stall" was ornith
  starved, never a real load.)
- Secondary: the HF repo serves bf16; the probe would stream **~33 GB** (cache
  holds only ~23.4 GB of the ~57 GB manifest) — a slow/stall-prone download even
  if fit weren't the wall.

**Verdict:** 2512 via vLLM-Omni can't render on a 32 GB V100 because the bf16
transient can't be held. A negative (OOM) result — that's the data the probe
was meant to surface. The robust path is sd.cpp + GGUF:
`stable-diffusion-cpp.md`.

## The reaper topology (why the stall hid for so long)

- `llama-cuda.service` is a **Type=oneshot** that disowns a manager
  `llama serve --port 8081 --models-max 1 --device CUDA0`. That manager keeps
  one model in VRAM and **respawns its serving child within seconds** — killing
  only the child triggers an instant respawn (the stale-mate).
- **Killing the manager tree (child + manager up to PID 1) frees the V100
  durably:** no systemd timer, no `olle` dependency, so nothing auto-restarts it.
- `olla.service` (:40114 → :8081) is the proxy; `llama-sycl.service` (Iris Xe,
  Qwen3.5-0.8B on :8082) does **not** use the V100 — leave it alone.
- **State now (2026-10-08 ~23:4x):** ornith's manager + child killed; V100 is
  **free (~9 MiB)** and not respawning. **ornith is currently DOWN** — restore
  with `sudo systemctl --system start llama-cuda` (or `switch-ai-backend.sh
  instruct`) **only when Jan says so**; the probe must not restart it.

## Clean-up checklist (do later — not now)

Order is optional; nothing here is time-critical.

- [ ] **Archive the dead probe scripts.** `probe1b_generate.sh` (original,
      broken — stray `.$WORK\`, undefined `log`/`vram_mb`, no-op `holder_pid`,
      malformed `vram_mb{…})` parens), `probe1b_generate.norestart.sh`,
      `probe1b_generate.fixed.sh`. Keep one as a historical artifact (rename to
      `*.superseded.sh`) or delete — the kill_chain/ancestor-kill idea is the
      reusable bit, captured in `vllm-omni-2512-probe.md`.
- [ ] **Drop the stalled HF bf16 cache for 2512.**
      `~/.cache/huggingface/hub/models--Qwen--Qwen-Image-2512` (~23.4 GB partial,
      bf16). sd.cpp uses **GGUF**, not this. `hf cache prune` or
      `rm -rf` the `models--Qwen--Qwen-Image-2512` dir — **only if the shared
      bind-mounted cache is safe to prune** (it is bind-mounted; confirm nothing
      else needs these blobs first).
- [ ] **Remove the local 2512 stub.** `models/Qwen-Image-2512/` holds only
      `*.index.json` + configs (no weight blobs) — it never loaded. Delete unless
      you want the bf16 snapshot for another backend.
- [ ] **Leave the vLLM-Omni engine in place** — it is still the omni text/audio
      engine (§11). Do not uninstall the venv / `vllm-src` / the text_to_image
      example.
- [ ] **Restore ornith** (`sudo systemctl --system start llama-cuda`) when Jan
      gives the go — the box currently has no llama backend.
- [ ] **Re-point the image-gen increment at sd.cpp.** Thread E's "target model =
      Qwen-Image-2512 via vLLM-Omni" is now superseded — update the probe order
      in thread E to the sd.cpp progression (SDXL → FLUX.1-dev Q8_0 → 2512 Q8_0).

## Reusable insight

- A "stall" with no log progress past *weight loading* on a single-V100 box is
  almost always **another backend holding the GPU**, not a model fault — free the
  card and re-run to see the real error.
- vLLM-Omni `--quantization int8` is **not** memory-safe on a card too small for
  the bf16 source: it materializes bf16 first. For small single-GPU cards,
  pre-quantized GGUF (sd.cpp) beats on-the-fly quant.
