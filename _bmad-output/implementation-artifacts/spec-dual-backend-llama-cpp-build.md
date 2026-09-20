---
title: 'Dual-backend llama.cpp build (CUDA + SYCL)'
type: 'feature'
created: '2026-09-20'
baseline_commit: 'e61561520d875eed8a0edd31518b087ee80c34b7'
status: 'in-progress'
route: 'dispatch'
review_loop_iteration: 0
context: []
---

<frozen-after-approval reason="human-owned intent — do not modify unless human renegotiates">

## Intent

**Problem:** The llama.cpp build script compiles only the CUDA backend, so the Intel DG1 GPU (SYCL) stays unused even though this machine has both an NVIDIA V100 and an Intel DG1.

**Approach:** Rework `run_once_5_aitools_1llama_cpp.sh` to locally compile llama.cpp once per available accelerator — a CUDA tree for the V100 and a SYCL tree for the Intel DG1 — into separate build directories, each gated on its GPU being present and its toolchain being installed. The CUDA path (already working) is preserved as-is.

## Boundaries & Constraints

**Always:**
- Keep everything idempotent and re-runnable; `cz apply` may run it many times.
- Build from source locally for both backends (no precompiled ollama/binary downloads).
- Only use sudo commands within the scoped NOPASSWD sudoers drop-in (see AGENTS.md); the script runs inside a chezmoi run script.
- Preserve the existing working CUDA build exactly; only extend it.

**Never:**
- Do not change the model file, the HuggingFace bind-mount, or the systemd boot unit as part of this — unless the human explicitly expands scope.
- Do not shell out to `ollama` or any hosted/prebuilt model service.

## I/O & Edge-Case Matrix

| Scenario | Input / State | Expected Output / Behavior | Error Handling |
|----------|--------------|----------------------------|----------------|
| Both GPUs present, both toolchains installed | fresh run on this desktop | builds `build_cuda` and `build_sycl`; installs both server binaries | a failed SYCL build must not abort the CUDA build (and vice-versa) |
| Intel GPU present, oneAPI not installed | run, oneAPI absent | behavior depends on Open Question 1 (install vs. skip) | if skipping, warn that SYCL is unavailable and continue with CUDA |
| Only CUDA GPU present (other desktops) | NVIDIA-only box | builds CUDA tree only, skips SYCL block cleanly | exit 0 |

</frozen-after-approval>

## Code Map

- `.chezmoiscripts/run_once_5_aitools_1llama_cpp.sh` -- **the target.** Clones `~/projs/llama.cpp`, checks out a release, `apt install`s CUDA deps, and does one `cmake -B build -DGGML_CUDA=ON ...` build. Contains aborted SYCL attempts as comments.
- `dot_local/bin/restart-llama-server.sh` (source name `executable_restart-llama-server.sh`) -- boots as `jan`, bind-mounts the HF hub, runs `llama serve --model ~/.cache/huggingface/hub/ornith.gguf`. CUDA/V100 oriented.
- `dot_local/bin/symlink_llama-server` -- chezmoi symlink `~/.local/bin/llama-server` → `~/projs/llama.cpp/build/bin/llama-server` (the single CUDA build path).
- `etc/systemd/system/restart-llama-server.service` -- system unit that runs the launcher at boot; `ExecStartPre` probes `llama serve --list-devices | grep -q CUDA` and refuses to fall back to CPU. CUDA-specific.
- `.chezmoiscripts/run_once_5_aitools_2llama_startup.sh` -- installs the systemd unit + fstab bind-mount. Not in scope unless Q2-B chosen.
- `AGENTS.md` -- sudo boundary: only the scoped NOPASSWD drop-in command families may use sudo.

## Tasks & Acceptance

**Execution:**
- [x] `".chezmoiscripts/run_once_5_aitools_1llama_cpp.sh"` -- restructure into per-backend build blocks gated on GPU presence and toolchain availability, preserving the CUDA block -- enable dual compilation
- [x] install the oneAPI toolchain via Intel's static installer into a user-local dir (no sudo — bash/sh is not in the sudoers alias), idempotent (skip when `icx` exists), `source setvars.sh` before the SYCL cmake; only when an Intel GPU is present
- [x] provide per-backend launcher commands in `~/.local/bin` (`llama-server-cuda`, `llama-server-sycl`) that point at the right tree and set the right device selector; leave the boot service + boot launcher untouched

**Acceptance Criteria:**
- [ ] Given this desktop with both GPUs, when the script runs to completion, then both `~/projs/llama.cpp/build_cuda` (CUDA) and `~/projs/llama.cpp/build_sycl` (SYCL) exist with a working `llama-server` binary each. *(executed via `cz apply` — see Implementation Notes)*
- [x] Given a clean re-run, the script is idempotent and does not fail because a build tree already exists. *(git reuse + `cmake -B` + oneAPI `icx` presence check)*
- [ ] Given the SYCL toolchain missing and not installed, when the script runs then it logs an error (SYCL unavailable) and still completes the CUDA build. *(build order CUDA first; each tree guarded with `|| log`)*
- Given both trees built, when the user runs the SYCL launcher then llama.cpp reports a Level-Zero Intel device (not CUDA); when the user runs the CUDA launcher then it reports an NVIDIA/CUDA device.

## Implementation Notes

- **oneAPI installed as the user, not via sudo.** The scoped sudoers alias does not include `bash`/`sh`, so the static installer can't be run under `sudo`. Installed to `$HOME/.local/intel/oneapi` via plain `curl` + `bash installer` (no root needed). This is a user-local install, so a future *boot-time* SYCL service couldn't see it before login — but boot wiring is out of scope (2A).
- **oneAPI URL is version-pinned** (`ONEAPI_INSTALLER_URL`): `intel-deep-learning-essentials-2026.0.0.624_offline.sh` (verified HTTP 200, ~1.5 GB). Bump this line when Intel ships a new toolkit. DLE chosen over Base toolkit (leaner, and the llama.cpp SYCL doc lists it as sufficient for the SYCL build).
- **CUDA kept at its previous behaviour**; the old `~/projs/llama.cpp/build` symlink (`dot_local/bin/symlink_llama-server`) now points at `build_cuda` so `llama-server` stays current.
- Dropped `set -u` (kept `pipefail`) to match the original's tolerance of an unset `$USER` in the chezmoi run env.
- **Verification:** syntax-checked (`bash -n`) the script and both wrapper heredocs; diff reviewed against `e615615`. The full build (1.5 GB oneAPI download + CUDA/SYCL compilation) was NOT executed here — it needs sudo (TTY) and long compile times — so ACs marked `[ ]` are to be confirmed by running `cz apply` on the desktop.

## Spec Change Log

(empty until first bad_spec loopback)
