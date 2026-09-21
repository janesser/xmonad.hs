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

## Second evolution: power (PWR) + temperature

The PMU detection fix only made the Iris Xe box appear — but the **power
reading was 0 W and there was no temperature**, because the DG1 does not
expose what btop expects:

| btop expected | reality on the DG1 |
|---|---|
| RAPL `energy-gpu` perf PMU (`gpu_power_open` → `/sys/devices/power/energy-gpu`) | **no `energy-gpu` event exists** — `power/events/` only has `energy-pkg` + `energy-ram`; RAPL layout also moved under `events/` |
| temperature support | `temp_info` was hard-disabled for Intel GPUs |

The **i915 hwmon driver** (`/sys/class/hwmon/hwmon5`, `name` = `i915`,
`device -> ../../../0000:2f:00.0`) reports what we need instead:

| sysfs file | meaning | sample |
|---|---|---|
| `energy1_input` | cumulative energy counter, microjoules | advanced ~2.85 µJ/µs → ~2.9 W at idle |
| `temp1_input` | die temperature, **milli-Celsius** | `54000` → 54.0 °C |
| `power1_rated_max` / `power1_max` | power ceiling | 28 W / 25 W |

### How it's wired
- `intel_gpu_top.h`: added `hwmon_present`, `hwmon_path[64]`, `hwmon_energy`
  (a `pmu_counter` holding the µJ counter) and `temp_milli` to `struct engines`.
- `intel_gpu_top.c`: `hwmon_find()` scans `/sys/class/hwmon/*/name`, matches
  `i915`, and confirms the `device` symlink's PCI address (derived from the
  resolved perf name `i915_0000_2f_00.0` → `0000:2f:00.0`) so the wrong sensor
  can't be read on a multi-GPU host. `pmu_init()` records the path; `pmu_sample()`
  refreshes `hwmon_energy` + `temp_milli` every cycle.
- `btop_collect.cpp` (`Intel::collect`): power = `dE(µJ) / dt(s) / 1e6` → W,
  fed into `pwr_usage` (mW) and the `gpu-pwr-totals` meter. It is a **fallback**
  — only used when `!r_gpu.present` (RAPL `energy-gpu` absent, i.e. the DG1);
  GPUs with a working RAPL GPU PMU keep using it unchanged. `temp1_input` →
  `temp` in Celsius (btop renders via `celsius_to`) with `temp_max = 100`;
  `temp_info` is enabled only when hwmon is present (so hwmon-less Intel GPUs
  are unaffected). The perf-sampled interval `t` (already computed for the
  other rate counters) is reused as `dt`, avoiding per-cycle state.

The whole thing is gated on `pmu_init()` succeeding — the existing precondition
for the Iris Xe box to appear at all — so `btop` still needs
`CAP_PERFMON` (`setcap cap_perfmon+ep /usr/bin/btop`).

Verified standalone on this host: `hwmon_find()` matched the DG1, power read
~2.9 W, temp 45 °C. (The perf path still needs `CAP_PERFMON`; `pmu_init` is
the pre-existing precondition that already works with the setcap in place.)

### Commits (on `btop-intel-gpu-fix`, branch `main ← janesser:btop-intel-gpu-fix`)
- `1455a80` read DG1/Iris Xe power+temp via i915 hwmon (driver + collect wiring)
- `fc0f87d` derive DG1 power from the perf-sampled interval + RAPL/temp guards

Candidate for a follow-up MR (or an addition to [#1848](https://github.com/aristocratos/btop/pull/1848)); not yet pushed/opened.

## Deployment
Installed by chezmoi run script
`.chezmoiscripts/run_once_5_aitools_3btop_intel_gpu_cap.sh` (run_once). Policy:
the patched btop is installed **only** where an Intel GPU is present; every
other host keeps the vanilla system btop. Two layers enforce this — a
top-level `has_intel_gpu()` guard, plus a defence-in-depth assertion inside
`ensure_btop()` that refuses to overwrite `/usr/bin/btop` when no Intel GPU is
present.

- Deployed binary: `/usr/bin/btop` = `1.4.6+975e395`, `cap_perfmon=ep`.
  Next deploy target after the power/temp evolution: `1.4.6+1455a80`.
- Build/MR source: `~/projs/btop`, branch `btop-intel-gpu-fix`.

## Merge request
Upstream PR **[#1848](https://github.com/aristocratos/btop/pull/1848)** —
`linux: detect i915 GPU on kernels that name the perf device by PCI address`
(`janesser`, co-authored with pi-agent), branch `main ← janesser:btop-intel-gpu-fix`,
`+37/−3`, 1 file.
