# Olla / llama.cpp — CUDA-only backend (XMG Core 15, AMD M20)

Host: **lincopta** — MSI/XMG Core 15 (AMD M20).

| component | value |
|---|---|
| NVIDIA GPU | GeForce RTX 2060 Mobile (TU106M), PCIe `01:00.0` |
| iGPU | AMD Renoir Radeon RX Vega 6 (`05:00.0`) |
| Intel GPU | **none** |
| llama.cpp backend | **CUDA only** (`~/.local/bin/llama-server-cuda`, `restart-llama-cuda.sh`) |
| Proxy | Olla `ollama.service` — public `:40114` → `127.0.0.1:8081` |
| Model (this machine) | **ornith-1.5 9B** |

There is **no SYCL / Intel backend here** — the box has no Intel GPU, so only
the CUDA backend is provisioned and routed. The SYCL pieces in the repo are kept
for machines that *do* have an Intel GPU and are gated behind `has_intel_gpu()`.

## Recent change: made the stack configurable + CUDA-only

Scripts that used to hardcode the GPU and the model are now parameterised. On
this box the defaults are already CUDA-only with the ornith-1.5 9B model.

- `dot_local/bin/executable_restart-llama-cuda.sh`
  - `LLAMA_HOST` (default `127.0.0.1`), `LLAMA_PORT` (default `8081`)
  - `LLAMA_MODEL_NAME` (default `ornith-1.5-9B.gguf`). Resolved in this order:
    a local path → a `.gguf` symlink under the HF cache → an HF repo
    `user/model[:quant]` (served from its cached blob, or `--hf-repo` if uncached).
- `.chezmoiscripts/run_once_5_aitools_1llama_cpp.sh` — new `LLAMA_BACKENDS`
  (space list, e.g. `cuda sycl`). Defaults to what the box has; `export
  LLAMA_BACKENDS=cuda` forces a CUDA-only build.
- `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh` — same `LLAMA_BACKENDS`;
  the `llama-sycl.service` unit is deployed only when an Intel GPU is present
  AND requested.
- `dot_config/olla/config.yaml` — single `:8081` CUDA endpoint; the old `:8082`
  SYCL endpoint was removed.
- `etc/systemd/system/ollama.service` — Description is now CUDA-only.

## Serving the model

The model blob is **not yet downloaded**. Point the launcher at it one of two
ways:

1. symlink (jan's convention):
   `~/.cache/huggingface/hub/ornith-1.5-9B.gguf` → the downloaded `.gguf` blob
   (matches `LLAMA_MODEL_NAME=ornith-1.5-9B.gguf`).
2. HF repo on first use:
   `export LLAMA_MODEL_NAME=<user>/<Ornith-1.5-9B-GGUF>[:Q4_K_M]`.

Start the backend with the systemd unit `llama-cuda.service` (enabled at boot),
or by hand `~/.local/bin/restart-llama-cuda.sh`.

## Gotchas

- **`ollama.service` is a phantom git entry.** `ls` lists it with full stat
  details, but `stat`/`cat`/`read`/`cp`/`git add` fail with ENOENT on the
  literal path; a shell glob (`etc/systemd/system/o*`) resolves it fine. Operate
  on it via the glob, not the literal name.
- The SYCL launcher `restart-llama-sycl.sh` and `llama-sycl.service` still exist
  in the repo for Intel boxes; they are **not** deployed or enabled here.
