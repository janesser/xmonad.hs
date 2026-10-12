# ai-client-only.md — peacewagon is a client-only box (no AI backend)

**Status: HELD** — core work done, one leftover with an open decision (see
below). 2026-10-12.

## Context
`peacewagon` is an Intel-only laptop (Kaby Lake, HD Graphics 615). No NVIDIA,
and the iGPU is "HD Graphics 615", not "Iris" — so `has_intel_gpu()` in
`~/.local/share/gpu.func` returns false. It is **not** an AI / llama backend
box. (The AI boxes are `lincopta` = NVIDIA+AMD, `cyberkleiber` = NVIDIA V100 +
Iris Xe.)

Earlier the llama boot-backend deploy script (`.chezmoiscripts/run_once_5_aitools_2llama_startup.sh`)
hard-coded `llama-cuda` into its backend table and gated only SYCL on physical
GPU presence — CUDA was deployed whenever the launcher symlink existed, regardless
of whether an NVIDIA GPU was present. On peacewagon that meant an NVIDIA-only
backend was force-installed and enabled, and `llama-cuda.service` sat in a
CUDA-probe restart loop (failing every 30s).

## Done (2026-10-12)
- **GPU-gated the deploy script.** CUDA now deploys only when `has_nvidia()`,
  SYCL only when `has_intel_gpu()`; `LLAMA_BACKENDS` only *selects*, physical
  presence *authorizes*. Verified it no-ops cleanly on peacewagon.
- **One-time teardown** (`run_once_9_cleanup_llama.sh`): removed
  `llama-cuda.service` + `llama-sycl.service` and the HuggingFace-cache bind
  mount from `/etc/fstab`. Guarded to a no-op on AI boxes. systemd sees no llama
  units now; the restart loop is dead.
- Removed the user-owned (non-chezmoi) llama files in `~/.local/bin`:
  `llama-server-cuda`, `llama-server-sycl`, `llama-server-restart.sh`,
  `pi-update-llama-models.sh`, `revise-llama-models-preset.sh`, and
  `~/.local/share/llama-cpp/lib.sh`.

## Leftover — the standing issue
The **chezmoi-managed** llama files in `~/.local/bin` are still recreated by
every `cz apply`:
- symlinks `llama`, `llama-bench`, `llama-fit-params`, `llama-server` →
  `~/projs/llama.cpp/build_cuda/bin/…` (now **dangling** — the build is gone),
- launchers `restart-llama-cuda.sh`, `restart-llama-sycl.sh`,
- the generic `llama-emergency*.sh`, `llama-handover*.sh`, `llama-integration-test.sh`.

They come back because the sources under `dot_local/bin/`
(`executable_restart-llama-cuda.sh`, `symlink_llama`, …) are **shared across all
hosts** — `lincopta` and `cyberkleiber` still need them. Deleting the sources
would break those AI boxes, so they stay in the repo. On peacewagon they are
harmless (nothing runs the launchers; the symlinks are just dangling), but they
are visual/operational overhead that contradicts "peacewagon is not a llama box".

## Open decision — which one?
1. **Leave it.** Harmless dangling links + unused launchers. Zero repo change.
2. **`remove_if` guards on the shared sources** (the clean "chezmoi respects
   that" fix): drop the CUDA symlinks + `restart-llama-cuda.sh` when
   `!has_nvidia`, and `restart-llama-sycl.sh` when `!has_intel_gpu`. All current
   AI boxes have NVIDIA, so this is safe for them; peacewagon loses the links.
   (Needs an explicit go-ahead — it edits shared sources that affect
   `cyberkleiber`/`lincopta`.)
3. Remove the sources entirely — **rejected**: breaks the AI boxes.

Prefer (2) if you want peacewagon fully clean; (1) is fine if the dangling links
don't bother you.

## Note
The live HF bind mount (`~/.cache/huggingface/hub`) is still mounted in the
running session (inert — nothing serves `:8081`; won't return at boot, the
fstab line is gone). It can't be unmounted via the scoped sudoers drop-in
(`umount` isn't in it); `sudo umount` by hand if you want it fully gone.
