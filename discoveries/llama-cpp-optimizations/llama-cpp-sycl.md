# llama.cpp: enable CUDA + SYCL (joined backends)

**Date:** 2025-09-08
**Status:** discovered + planned, not yet executed
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
