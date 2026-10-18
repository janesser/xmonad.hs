# Upgraded llama.cpp primary backend to v0.6.0 (cyberkleiber)

> **Status: done (2026-10-12)** — v0.6.0 engaged as the live llama-cuda backend
> on the V100 (:8081), implemented + verified, rollback path preserved.

## What changed

llama.cpp on cyberkleiber was upgraded from **v0.5.0 to v0.6.0** as the primary
`llama-cuda` backend (the V100 behind the Olla/router :8081). The engage was
deliberately **non-disruptive**: the smoke test ran on a throwaway clone/port,
then a handover handed the V100 to `llama-sycl` (:8082) briefly while the
production binary was rebuilt, after which llama-cuda came back on v0.6.0.

### How it was done

1. **Pinned the main clone** `~/projs/llama.cpp` to the `v0.6.0` tag
   (commit `d81235049`). The clone was on v0.5.0 before this.
2. **Canonical rebuild** of `~/projs/llama.cpp/build_cuda` from v0.6.0 source,
   using the *exact* original v0.5.0 CUDA CMake flag set (GGML_CUDA=ON, NCCL,
   tensorrt FA, GRAPHS, `GGML_CUDA_COMPRESSION_MODE=size`, FA quants; **F16 stays
   OFF** — the original build did not set `GGML_CUDA_F16`). Build 11429.
   (A throwaway `build/` clone was built with F16 *on* for the early smoke test;
   that was the non-canonical variant and has since been removed.)
3. The production launcher `restart-llama-cuda.sh` execs `llama` from PATH →
   `~/.local/bin/llama` → `build_cuda/bin/llama`, so it now runs v0.6.0
   automatically. The `~/.local/bin/llama` symlink was untouched.
4. **Rollback preserved**: `~/projs/llama.cpp/build_cuda.bak` is the v0.5.0
   build. To roll back: `mv build_cuda build_cuda.v060 && mv build_cuda.bak
   build_cuda`, then `systemctl restart llama-cuda` (after a normal
   llama-sycl handover).

### Verification

- `:8081` reports the v0.6.0 **system_fingerprint** `b11429-d81235049`
  (build 11429 + commit d81235049) — this is the commit hash embedded in the
  v0.6.0 build tag, so a fingerprint that no longer matches is a v0.6.0
  regression signal.
- The 35B-A3B (Ornith-1.5-35B-A3B) loads on the V100 and serves on CUDA0.
- After jan's manual `systemctl restart` of the units, `:8081` still served the
  v0.6.0 binary (`/home/jan/projs/llama.cpp/build_cuda/bin/llama`); the 9B
  (Ornith-1.5-9B) was the currently loaded model (the 35B is unloaded and
  re-loadable on demand).

### Behavioural note for clients

v0.6.0 returns a parallel **`reasoning_content`** field on chat completions for
the reasoning models (e.g. the 35B-A3B), separate from `content`.
`system_fingerprint` includes the commit hash — worth surfacing to any client
parsing the response.

## Side effect: llama-sycl brought under systemd management

While doing this work, `:8082` (`llama-sycl`, the Intel-GPU companion /
llama-cuda handover target) was running as an **orphaned manual process**
(serving `LiquidAI/LFM2.5-8B-A1B-GGUF:Q4_0`) while its systemd unit
`llama-sycl.service` was inactive/dead. The orphan was untracked, so it had no
self-heal and wasn't boot-managed. The systemd unit's launcher (`restart-
llama-sycl.sh`) serves the **same** model/binary, so the orphan was replaced by
a properly systemd-managed process (`systemctl start llama-sycl`):

- `:8082` now served by a systemd-tracked process (PPID=1), **Restart=on-failure**
  (self-heals on crash), **enabled at boot** (WantedBy=multi-user.target).
- Same LFM2.5-8B-A1B model — no behaviour change to the little-brother agent.
- The handover protocol (stop llama-cuda → `llama-sycl` takes over) still holds.

## Refs

- v0.6.0's auto-fit (default `--fit on`) made ctx-size a per-load runtime
  value — the pin/crossbar decision record (local, git-ignored):
  `../../_bmad-output/planning-artifacts/architecture/architecture-llama-ctx-crossbar-2026-10-16/DECISIONS.md`
  (no pin; client-side contextWindow knob instead).
- Sibling serving-robustness notes: `ai-server/ai-emergency-fallback.md`.
- The backend table (llama-cuda + llama-sycl deploy) is in the run-once
  `run_once_5_aitools_2llama_startup.sh`.
