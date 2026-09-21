# Iris Xe btop fix (HP Z6 G4)

The HP Z6 G4 has two GPUs: **Intel DG1 / Iris Xe** (`2f:00.0`) and **NVIDIA
Tesla V100** (`21:00.0`). By default btop only showed the V100 — the Iris Xe
box silently vanished. Fixed by patching + rebuilding btop.

## Symptom
`btop`'s gpu box showed the NVIDIA V100 but not the Intel Iris Xe.
`shown_boxes = "cpu mem net proc gpu0 gpu1"` still only rendered one gpu box.

## Root cause
btop 1.4.6 hardcodes the i915 perf-PMU name as `i915`
(`src/linux/intel_gpu_top/intel_gpu_top.c`). On this kernel (7.0.x) the i915
perf PMU is named by PCI address (`i915_0000_2f_00.0`), so btop looks at the
missing paths:

| btop reads | reality |
|---|---|
| `/sys/devices/i915/events` (discover_engines) | absent — real is `/sys/devices/i915_0000_2f_00.0/events` |
| `/sys/bus/event_source/devices/i915/type` (pmu_init) | absent — real is `…/i915_0000_2f_00.0/type` (=58) |

`discover_engines` returns NULL → "Failed to find Intel GPU engines" → Iris Xe
never detected. The V100 (NVML) needs no cap and was never affected.

## Fix
- Patch `discover_engines()`: scan `/sys/devices` for an `i915*` dir exposing an
  `events/` subdir, and propagate the resolved instance name (`i915_0000_2f_00.0`)
  into `engines->device` so `pmu_init()` finds the perf event source type. Add
  `free(engines->device)` on both cleanup paths.
- Grant `/usr/bin/btop` `CAP_PERFMON` (attaching to the i915 perf PMU via
  `perf_event_open` needs it; `kernel.perf_event_paranoid = 4`).
  `sudo setcap cap_perfmon+ep /usr/bin/btop`.

Both GPUs then render as separate boxes: `gpu0` = V100, `gpu1` = Iris Xe.

## Deployment
Installed by chezmoi run script
`.chezmoiscripts/run_once_5_aitools_3btop_intel_gpu_cap.sh` (run_once). Policy:
the patched btop is installed **only** where an Intel GPU is present; every
other host keeps the vanilla system btop. Two layers enforce this — a
top-level `has_intel_gpu()` guard, plus a defence-in-depth assertion inside
`ensure_btop()` that refuses to overwrite `/usr/bin/btop` when no Intel GPU is
present.

- Deployed binary: `/usr/bin/btop` = `1.4.6+975e395`, `cap_perfmon=ep`.
- Build/MR source: `~/projs/btop`, branch `btop-intel-gpu-fix`.

## Merge request
Upstream PR **[#1848](https://github.com/aristocratos/btop/pull/1848)** —
`linux: detect i915 GPU on kernels that name the perf device by PCI address`
(`janesser`, co-authored with pi-agent), branch `main ← janesser:btop-intel-gpu-fix`,
`+37/−3`, 1 file.
