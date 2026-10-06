# llama.cpp: enable CUDA + SYCL (joined backends)

**Date:** 2025-09-08
**Status:** **RESTORED (router mode) 2026-10-06** — launcher + systemd unit
rewritten to match `llama-cuda` (commit `ee5bdd9`'s refactor); **build itself
still pending** (one-time oneAPI + SYCL compile) — see below.
**Target box:** xmonad desktop — NVIDIA Tesla V100 32GB + Intel DG1 Iris Xe

## Goal

Make the user's llama.cpp able to use **both** GPUs (V100 via CUDA, Intel DG1 via
SYCL) from a **single binary** — backends "joined".

## Working copy guardrail

`~/projs/llama.cpp/` is the pristine, working checkout. It **must stay immutable**.
Do **not** run cmake, make, or build inside it. All work happens in a separate
tree: `~/projs/llama.cpp-cc/`.

## Current state (verified)

| Item | Value |
|------|-------|
| Build checkout | `~/projs/llama.cpp`, HEAD `b10855` (detached), clean working tree |
| CUDA in current build | `GGML_CUDA=ON` (nvcc 12.4, driver 580.178) |
| SYCL in current build | `GGML_SYCL=OFF` |
| Intel oneAPI compilers (`icx`/`icpx`) | **missing** — required to build the SYCL backend |
| Intel OpenCL ICD | present (`intel-opencl-icd 26.05`, `libigc`, `xe`/`i915` kernel modules) → DG1 should be visible at runtime |

## Can backends be joined? — YES (verified in source)

`ggml/src/ggml-backend-reg.cpp` implements `ggml_backend_load_all()`, which loads
**every** `ggml-*.so` present in the executable's own directory:

```cpp
void ggml_backend_load_all_from_path(const char * dir_path) {
    ggml_backend_load_best("cuda",  silent, dir_path);
    ggml_backend_load_best("sycl",  silent, dir_path);
    ggml_backend_load_best("cpu",   silent, dir_path);
    // ... hip, metal, vulkan, opencl, ...
}
```

`main` calls `ggml_backend_load_all()`, so one host binary picks up `ggml-cuda.so`
(V100) **and** `ggml-sycl.so` (DG1) at once. This is exactly how the release
binaries ship: one host binary, backend `.so` files dropped beside it.

## Build plan

Two separate CMake trees, merged into one bin dir (this is the same "merge
artifacts" step CI uses):

1. **Tree A — gcc + nvcc** (host binary + CUDA backend)
   - build host `llama`/`llama-server`/`llama-cli` + `ggml-cuda.so`
2. **Tree B — icx/icpx** (SYCL backend only)
   - build **only** the `ggml-sycl` target (no host binary needed) → `ggml-sycl.so`
3. **Merge** into `~/projs/llama.cpp-cc/bin/`:
   - host binaries + `ggml-cuda.so` + `ggml-sycl.so`
   - bundle oneAPI runtime libs next to the sycl `.so` (see caveat 2)

Run: `~/projs/llama.cpp-cc/bin/llama-server -m model.gguf` — both GPUs active.
Layer placement: `--gpu-layers` / `--split` / auto-balance.

## Why two trees (not one configure)

SYCL forces the global C++ compiler to `icx`, which cannot compile the `.cu`
CUDA files (those need nvcc). A single `cmake .` picks one C++ compiler, so
`ggml-cuda.so` and `ggml-sycl.so` cannot be produced in one configure. Build
separately, merge the `.so`.

## Caveats / risks

1. **Cross-toolchain `dlopen`:** gcc host loading an icx-built `.so` is ABI-stable
   via the `ggml_backend` C interface, but both must share the same `libstdc++`
   major version. Verify at runtime.
2. **oneAPI runtime at run time:** the `.so` needs its runtime libs
   (`libintel_opencl.so`, TBB, etc.). The DG1 OpenCL ICD is already installed, so
   the OpenCL path should work; drop the oneAPI shared libs beside the binary or
   set `LD_LIBRARY_PATH`. (CI separately installs the Level Zero SDK if you want
   the Level Zero path instead.)
3. **DG1 support:** SYCL targets Intel Arc / Flex / Max / iGPU. DG1 (PVC) is not
   on the official supported list, but the present OpenCL ICD makes it plausibly
   usable — verify at runtime with `--list-gpus` / device selection.

## Open decision before executing

Install oneAPI compilers: **user-local** (`~/.local/oneapi`, no `sudo`) vs
**system-wide** (`/opt/intel`, needs `sudo` — outside the normal sudoers boundary,
needs explicit user OK). Default recommendation: user-local.

## Execution (restored 2026-10-06, aligned to the `llama-cuda` router refactor)

The build was never completed (see the pending note below), so this section
records how the **launcher + unit** were brought in line with the CUDA backend's
last state (`ee5bdd9`), not the old fixed-model `llama-server` style.

- **Launcher** `~/.local/bin/restart-llama-sycl.sh` (chezmoi source
  `dot_local/bin/executable_restart-llama-sycl.sh`) now runs **router mode** via
  the unified `llama serve` CLI — the same pattern as
  `restart-llama-cuda.sh`:
  - `--host 127.0.0.1 --port 8082 --models-max 1 --parallel 1 --device SYCL0 --no-ui`
    (fork + disown, exits so the systemd `Type=oneshot` tracks only the launcher;
    the orphaned server keeps serving; the next run reaps the stale one).
  - **Device pin = `SYCL0`.** CUDA uses `--device CUDA0`; the Intel backend's
    own index is `SYCL0`. Confirm the exact index after the build with
    `$BUILD_SYCL/bin/llama serve --list-devices` and fix the launcher if it
    differs. Auto-select is left on (no `ONEAPI_DEVICE_SELECTOR`); this is only
    the llama.cpp-side pin.
  - Default model (LFM2.5-2.6B) is exposed to the router via a tidy symlink
    (`~/.cache/huggingface/hub/LFM2.5-2.6B.gguf`), mirroring the CUDA ornith
    symlink, so Olla discovers it from the HF cache — no `--hf`, no fixed path.
  - Logs go to `journalctl` (no `--log-file`), like CUDA.
  - `--device` is the only functional change vs. the old launcher besides the
    router mode; everything else (oneAPI sourcing, render/video group adds,
    port-bound reaper, `set -u` avoidance) is carried over.
- **Unit** `llama-sycl.service` (chezmoi
  `etc/systemd/system/llama-sycl.service`, installed by
  `run_once_5_aitools_2llama_startup.sh`) now:
  - calls the launcher `restart-llama-sycl.sh live 8082` (the new signature),
  - has an `ExecStartPre` that **fails fast** if no SYCL device enumerates
    (mirrors the CUDA probe) and **skips cleanly** if the build is absent, so a
    fresh boot does not fail-loop, and
  - does **not** bootstrap the build from systemd (the multi-minute
    download+compile would blow the 120s oneshot window and loop on every boot);
    the launcher bootstraps only when run interactively, so the build is a
    one-time manual step.
- **Deploy order:** `cz apply` (installs/units the unit, enabled at boot but
  `START=no` → not started on deploy) → **manual one-time build** →
  `systemctl restart llama-sycl`.

### Still pending: the build itself

`build_sycl/` does not exist, so no SYCL binary is present to run. First run of
the launcher (or any `llama-sycl-test.sh`) triggers the one-time bootstrap in
`~/.local/share/llama-cpp/lib.sh`: download the ~1.5 GB Intel Deep Learning
Essentials installer → `~/.local/intel/oneapi` (no sudo) → cmake build of the
`build_sycl` tree. Until that completes the unit's `ExecStartPre` skips and the
backend is unreachable. The oneAPI install is user-local (per the 2025-09-08
open decision), no `sudo`.

## Reference

CI SYCL recipe (the line the user linked), `.github/workflows/release.yml`:

```bash
source /opt/intel/oneapi/setvars.sh
cmake -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DGGML_SYCL=ON \
  -DCMAKE_C_COMPILER=icx \
  -DCMAKE_CXX_COMPILER=icpx \
  -DCMAKE_INSTALL_RPATH='$ORIGIN' \
  -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON \
  -DLLAMA_OPENSSL=OFF \
  -DGGML_NATIVE=OFF \
  -DGGML_SYCL_F16=ON
cmake --build build --config Release -j "$(nproc)"
```
