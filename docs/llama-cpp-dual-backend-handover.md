# llama.cpp Dual-GPU Build — Handover

**Date:** 2026-09-20
**Repo:** `~/.local/share/chezmoi` (dotfiles) · **Target machine:** X11 desktop, NVIDIA Tesla V100 (32 GB) + Intel DG1 (Iris Xe)
**Spec:** `_bmad-output/implementation-artifacts/spec-dual-backend-llama-cpp-build.md` (status: *in-progress*)

---

## 1. Where things stand (TL;DR)

✅ **Functionally complete and working — but ONLY on disk.** Both backends build from
`origin/master` and both wrappers serve `--cache-list` (`hf cache list`) on their own GPU.

✅ **The fix IS now persisted** (applied 2026-09-20): the build script checks out
`origin/master` and generates the correct wrappers (dedicated `llama-server` binary,
auto SYCL device select). Verified end-to-end — both wrappers serve `--cache-list`.

Note the build still runs from `origin/master` (unpinned); see the pinning question in §5.

---

## 2. What works now (verify)

```bash
# CUDA (V100)
~/.local/bin/llama-server-cuda --cache-list          # → CUDA0: Tesla V100-SXM2-32GB, 18 models, exit 0
~/.local/bin/llama-server-cuda --list-devices

# SYCL (Intel DG1 / Iris Xe)
~/.local/bin/llama-server-sycl --cache-list          # → SYCL0: Intel Iris Xe Graphics, 18 models, exit 0
~/.local/bin/llama-server-sycl --list-devices

# General tools (all repointed to the CUDA build)
llama / llama-bench / llama-fit-params / llama-server   # → $HOME/projs/llama.cpp/build_cuda/bin/*
```

**Build state (on disk):**
- Source: `~/projs/llama.cpp` at `origin/master` = `ce8caa6e6` (llama.cpp build **11065**).
- `build_cuda/` (master, ccache build, ~fast recompile) and `build_sycl/` (master, oneAPI).
- oneAPI at `~/.local/intel/oneapi` (icx 2026.0.0). CUDA toolkit 12.4, V100 driver loaded.
- Removed the orphaned `build/` tree (stale b11064) — reclaimed **807 MB**.

**HF cache:** hub is bind-mounted at `~/.cache/huggingface/hub`; 18 models listed by both backends.

---

## 3. The critical gap (top next item)

The working state is on **ephemeral paths**; the repo does not reproduce it. `git status`
shows staged changes, but the staged `run_once_5_aitools_1llama_cpp.sh` still:

| Concern | Staged (broken) | Working (on disk) |
|---|---|---|
| Source checkout | `git tag … | head -1` → **b11064** | **`origin/master`** (needs `--cache-list`) |
| Wrapper `exec` target | `…/build_cuda/bin/llama` (unified) | **`…/build_cuda/bin/llama-server`** (has `--cache-list`) |
| SYCL selector | `ONEAPI_DEVICE_SELECTOR=level_zero:0` (fails: "No device of requested type") | **auto-select** (DG1 not a Level-Zero device here) |

**To persist (next commit):** in `run_once_5_aitools_1llama_cpp.sh`
1. checkout `origin/master` instead of the latest *tag*;
2. in `write_generated_files`, make the wrappers exec the dedicated `llama-server`
   binary and drop the forced `level_zero:0` selector (auto, overridable via env).

Until that is done, the safe state is: **leave `~/projs/llama.cpp` at master and do not run the script.**

---

## 4. What is persisted in the repo (staged, uncommitted)

- `dot_local/bin/symlink_llama` / `symlink_llama-bench` / `symlink_llama-fit-params`
  → repointed to `…/build_cuda/bin/*` (was `…/build/bin/*`, stale b11064).
- `dot_local/bin/symlink_llama-server` → `…/build_cuda/bin/llama-server` (done earlier).
- `.chezmoiscripts/run_once_5_aitools_1llama_cpp.sh` — the *pre-wrapper-fix* rewrite
  (runtime-owns the GPU detection + CUDA build; generates the wrappers/lib). **needs §3 fix.**
- `_bmad-output/implementation-artifacts/spec-dual-backend-llama-cpp-build.md` — spec.

> `~/.local/bin/llama-server-{cuda,sycl}` and `~/.local/share/llama-cpp/lib.sh` are
> **runtime-generated, not git-tracked.** Editing them on disk does not change the repo;
> they are (re)written by the run script. That is exactly why §3 must fix the generator.

---

## 5. Open / remaining

1. ✅ Persist the §3 fix into `run_once_5_aitools_1llama_cpp.sh` (master checkout + correct wrappers) — **done 2026-09-20**.
2. **Commit** the staged changes (symlinks, spec, script) — the pending step.
3. **Pinning decision:** `origin/master` is unpinned (moves over time). A newer tagged release
   with `--cache-list` does **not** exist in this mirror (tags stop at `b11064`), so master is
   the only option here. Choose: keep master, or pin a fixed commit if the mirror ever gets one.
4. Spec is `in-progress`; remaining BMAD steps (review / finalize) if this continues under bmad-build.

---

## 6. How the key problems were solved (for the next person)

- **`hf cache list` is not a command** — the real flag is `llama-server --cache-list`
  (or `-cl`). The *unified* `llama` binary rejects it at the top level (first token = command).
  So the `llama-server-*` wrappers must exec the **dedicated `llama-server`** binary, not `llama`.
- **`--cache-list` needs `origin/master`.** It was added in ggml-org/llama.cpp PR #20775; the
  pinned tag `b11064` predates it. (`common/hf-cache.cpp`, `common/arg.cpp` register `-cl`.)
- **SYCL `level_zero:0` fails** on this box ("No device of requested type"); the DG1 is not
  exposed as a Level-Zero device. **Auto-select** targets the Iris Xe cleanly.
- **SYCL binary needs oneAPI on the lib path** (`libsvml.so`, etc.) — the wrappers source
  `~/.local/intel/oneapi/setvars.sh` before exec (already implemented).
- **`-q` in a middle `grep`** silently truncates pipelines — only the *last* grep in a chain
  may use `-q`. (`has_intel_gpu` in the run script.)
