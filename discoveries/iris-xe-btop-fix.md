# Iris Xe btop fix (HP Z6 G4 — cyberkleiber)

The HP Z6 G4 has two GPUs: **Intel DG1 / Iris Xe** (`2f:00.0`) and **NVIDIA
Tesla V100** (`21:00.0`). By default btop only showed the V100 — the Iris Xe
box silently vanished. Fixed by patching + rebuilding btop.

## Symptom
`btop`'s gpu box showed the NVIDIA V100 but not the Intel Iris Xe.
`shown_boxes = "cpu mem net proc gpu0 gpu1"` still only rendered one gpu box.

## Root cause
btop 1.4.6 hardcodes the i915 perf-PMU name as `i915`
(`src/linux/intel_gpu_top/intel_gpu_top.c`). On this kernel (7.0.x) the i915
perf PMU is named by PCI address (`i915_0000_2f:00.0`), so btop looks at the
missing paths:

| btop reads | reality |
|---|---|
| `/sys/devices/i915/events` (discover_engines) | absent — real is `/sys/devices/i915_0000_2f_00.0/events` |
| `/sys/bus/event_source/devices/i915/type` (pmu_init) | absent — real is `…/i915_0000_2f:00.0/type` (=58) |

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
  resolved perf name `i915_0000_2f:00.0` → `0000:2f:00.0`) so the wrong sensor
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

### Verified end-to-end (live, on this host)
Running the capped binary's real driver path (`pmu_init` + `pmu_sample`,
sample interval ~1.5 s) against `i915_0000_2f:00.0`:

| signal | value | meaning |
|---|---|---|
| `pmu_init` | `0` | perf-PMU attach succeeds **only** with `CAP_PERFMON`; without it `perf_event_open` returns `EACCES` (`num_gts=-13`) and the box is dead |
| `r_gpu.present` | `0` | confirms no RAPL `energy-gpu` → the hwmon fallback is the active path |
| hwmon | `hwmon5` (`/sys/class/hwmon/hwmon5`) | i915 sensor device found and PCI-matched |
| power | `4.64 µJ / 1.5 s → ~3.1 W` | live idle draw |
| temp | `temp1_input=50000` | `→ 50.0 °C` (driver divides by 1000) |

Both GPUs now render as separate boxes with util, frequency, **power (W)** and
temperature: `gpu0` = V100, `gpu1` = Iris Xe.

### Deployment gotchas (why a bare `cz apply` showed nothing)
Two independent failures hid the change. Do **not** assume `cz update`/`cz apply`
reaches either:

1. **The build is skipped after the first run.** The run script only builds when
   `~/projs/btop/build/btop` is absent. It already existed (the old
   discovery-only build), so later `cz apply` runs logged *"already built"* and
   never compiled or installed the new code. **Deploy the evolved binary
   manually** once the branch is ready (see below).
2. **The `CAP_PERFMON` grant had silently failed at initial setup** —
   `getcap /usr/bin/btop` came back empty. With `perf_event_paranoid=4` and no
   capability, `pmu_init()` fails (`EACCES`) and the Iris Xe box never gets real
   data (or at all). Re-apply it: `sudo setcap cap_perfmon+ep /usr/bin/btop`
   (persists as a file attribute).

Deploy run (branch already built locally at `~/projs/btop/build`):

```
sudo install -m755 ~/projs/btop/build/btop /usr/bin/btop
sudo setcap cap_perfmon+ep /usr/bin/btop
```

Both commands use the scoped `chezmoi-pi` NOPASSWD drop-in (`install`,
`setcap cap_perfmon+ep /usr/bin/btop`). Note `install` (and `cp`) replace the
inode and **strip** the file capability, so `setcap` must follow the install.

### Deploy state
- `/usr/bin/btop` = `1.4.6+975e395` (built from `btop-intel-gpu-fix`), `cap_perfmon=ep`.
- Build/MR source: `~/projs/btop`, branch `btop-intel-gpu-fix`.
- Manually deployed (see gotchas above) — `cz apply` does not rebuild it.
- `1455a80` read DG1/Iris Xe power+temp via i915 hwmon (driver + collect wiring)
- `fc0f87d` derive DG1 power from the perf-sampled interval + RAPL/temp guards

Candidate for a follow-up MR (or an addition to [#1848](https://github.com/aristocratos/btop/pull/1848)); not yet pushed/opened.

## Deployment
**Manually deployed** — the old run script (`run_once_5_aitools_3btop_intel_gpu_cap.sh`,
a once-script with a per-host Intel-GPU guard) was retired: no Iris Xe hardware
is expected on any host, and vanilla btop from the distro repo is now used
everywhere.

- Deployed binary: **retired** — the patched binary (`1.4.6+975e395`) was overwritten by a distro package update; `/usr/bin/btop` is now `1.4.7-1` from the repo.
- Build/MR source: `~/projs/btop`, branch `btop-intel-gpu-fix`.

## Merge request
Upstream PR **[#1848](https://github.com/aristocratos/btop/pull/1848)** —
`linux: detect i915 GPU on kernels that name the perf device by PCI address`
(`janesser`, co-authored with pi-agent), branch `main ← janesser:btop-intel-gpu-fix`,
`+37/−3`, 1 file.

## Deployment details (dual-gpu)
- **Deployed:** `~/.local/bin/btop` = `1.4.7+2ad5308`, `cap_perfmon=ep`, `jan:jan`
  — the patched binary; `/usr/bin/btop` stays the vanilla apt package.
- **`CAP_PERFMON` also required:** `sudo setcap cap_perfmon+ep ~/.local/bin/btop`
  (needs jan's sudo **password** — the NOPASSWD drop-in only names `/usr/bin/btop`)
- **Deploy script:** `.chezmoiscripts/run_once_5_aitools_3btop_intel_gpu_cap.sh`
  (run_once, tracked in git — **retired**: no Iris Xe hardware expected; now only
  manual deployment applies, see Deployment).
- **Dev clone:** `~/projs/btop` (branch `btop-intel-gpu-fix`), deploy uses a
  separate clean clone at `~/projs/btop-intel-gpu`.
- **Config:** `~/.config/btop/btop.conf` (`shown_gpus = "nvidia amd intel"`,
  `shown_boxes = "cpu mem net proc gpu0 gpu1"`).
- **Verification (headless):** run with `--debug` and confirm the log has **no**
  `Failed to find Intel GPU engines` and **no** `Failed to initialize PMU`.
