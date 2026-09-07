# Handover — Day 14 — Camera Debug Case Status & Next Steps

**Date:** 2026-09-07 (resume point for the chuwi_ubook_xpro camera case)
**Owner for handover:** next bmad session (PM/Architect/Dev)
**Location of artifacts:** `devices/chuwi_ubook_xpro/chuwi_camera_drivers/agent_work/`

> This supersedes `day13.md` (DMA recovery planning) — the case pivoted away from
> DMA work once live boot evidence showed the real blocker is **PMIC rail + MCLK**,
> not the DMA path. `day13.md` is kept for history; do not treat its "implement
> DMA error recovery" todo as current.

---

## 1. Git / repo hygiene (checked 2026-09-07)

`git status` on the chezmoi repo:

- `main` is **in sync with `origin/main`** (nothing lost).
- **1 tracked change:** `dot_pi/agent/encrypted_settings.json.age` (session config, ignore).
- **Untracked files in the camera case** (all belong to this case, safe to keep/commit
  together later):
  - `_driver-case-live-status.md` — live diagnosis (the current source of truth).
  - `_resume-todo.txt` — return-to notes with sudo installed + next experiments.
  - `chezmoi-pi-camera-debug.proposed` — **installed & validated** separate sudoers drop-in.
  - `chezmoi-pi.camerasudo.proposed` — older proposal (merged into the above; keep for diff).
  - `int3472-probe-failure-analysis.md` — Task B root-cause (partly superseded, see §2).
  - `todo.txt`, `INT3472-linux-analysis.md` — earlier notes.

Suggested: `git add` the untracked case files + commit as one logical unit once the
next experiment round is done (don't commit `encrypted_settings.json.age`).

---

## 2. Current diagnosis (the ground truth, live on kernel 6.8.0-139-generic)

The stack is **much further along** than the older analysis assumed. Key corrections:

- **`int3472-tps68470` I2C driver is NOT the active PMIC driver here.** On this kernel
  the discrete PMIC is served by **`int3472-discrete` (module alias `intc`)**, plus
  `tps68470_regulator` / `clk_tps68470`. (The I2C-driver root cause in
  `int3472-probe-failure-analysis.md` is stale for the *current* boot — that doc is
  kept as historical.)
- **Both pipeline parts bind cleanly at boot:**
  - `ipu3-imgu` binds the Imaging Unit at `00:05.0` ([8086:1919]), loads firmware, `ipu_bridge` up.
  - `ipu3-cio2` binds the CSI-2 host at `00:14.3` ([8086:9d32]); reports
    `Found supported sensor OVTI2680:00` / `Connected 1 cameras`.
  - So the old "`proc_thermal` grabbed IP3@00:04 so `ipu3-cio2` never binds" story
    **does not apply** on this kernel. **Do not run `fix/proc_thermal_release_ip3.sh`** —
    it was built on the now-obsolete premise.
- **Front camera OV2680 (CAM0)** binds: `/sys/bus/i2c/devices/i2c-OVTI2680:00/driver -> ov2680`,
  read `sensor_revision id = 0x2680`.
- **Rear camera OV5648 (CAM1):** `i2c-OVTI5648:00` exists (ACPI on I2C2, addr 0x36,
  modalias `acpi:OVTI5648`), but `ov5648` module was **not** loaded this boot. Whether
  this unit physically has a rear cam is unconfirmed.
- **16 `/dev/video*`** (media0 = cio2 → video0-3; media1 = ipu3-imgu → video4-13).

### Remaining issues (confirmed)

1. **Front cam binds but won't stream.** `v4l2-ctl -d /dev/video1 --stream-mmap` →
   `VIDIOC_STREAMON returned -1 (Link has been severed)`. Media graph shows the
   `ov2680 2-0010:0 -> ipu3-csi2 1:0` link **present but DISABLED** (no `[ENABLED]`).
   Leading hypothesis: discrete PMIC rails are **dummy** → sensor can't power up for
   CSI; plus no MCLK.
2. **Rear OV5648 not loaded** (see above).

---

## 3. Root-cause chain for "won't stream" (session-2 diagnosis)

`intc` binds `INT3472:01`/`:02` (status=15, enabled) but creates **no** regulator and
**no** Mclk for the camera. Why:

- `int3472-discrete` registers a real rail / GPIO clock **only** when an INT3472 PMIC's
  `_CRS` GPIO has a `_DSM` (GUID `79234640-…`) entry whose packed type field (bits 7:0)
  == `POWER_ENABLE` (0x0b) or `CLK_ENABLE` (0x0c). The map only exposes supplies
  `"avdd"`/`"AVDD"` (in `clk_and_regulator.c`).
- The DSDT INT3472 `PMIC-CRDG` device exposes `_CRS` GPIOs from `PINR(C0P#, C0G#)` and
  its `_DSM` returns `GPPI(C0F0, …)`. `_STA` is `0x0F` only if `CL00 && C0TP==1`,
  else hidden.
- Camera-serving controllers are **`INT3471` (CAM0)** and **`INT3474` (CAM1)**. Their
  `_DDN` still read `IMX135-CRDG2` / `OV2740-CRDG2` — the DSDT is **raw/unpatched
  firmware with old sensor names** (matches earlier note).
- **The `C0*` pin VALUES are `External` in the DSDT**, defined in a **power-config SSDT
  that is NOT in the repo** (grep found only External decls). So the actual GPIO
  pin/type can't be resolved statically from what we have.

**Conclusion:** the camera-serving INT3472 either isn't the enabled one, or its
`_DSM`/`GPPI` convention doesn't surface a `POWER_ENABLE`/`CLK_ENABLE` pin, so `intc`
never creates the rail → OV2680 stays on `reg-dummy`, gets no Mclk, and the media
link stays disabled → STREAMON "Link severed". Dummy rails + no clock = no streaming
even if the link were forced on.

---

## 4. Blockers & unblocks

- ✅ **SUDO INSTALLED (2026-09-07):** a separate, scoped, NOPASSWD drop-in
  `/etc/sudoers.d/chezmoi-pi-camera-debug` (repo copy: `chezmoi-pi-camera-debug.proposed`,
  `visudo` validated) grants passwordless sudo for the 10 camera-debug tools
  (`modprobe`, `rmmod`, `i2cdetect/i2cset/i2cget/i2ctransfer`, `media-ctl`,
  `acpidump`, `iasl`, `v4l2-ctl`). This clears the "no root to capture/rebind/verify"
  blocker.
  - **Constraint (honour this):** do NOT modify or broaden the existing `chezmoi-pi`
    rule (`/etc/sudoers.d/chezmoi-pi`) — the camera rule is a separate file by design.
- ⚠️ **Static resolution stalled:** the `C0*` pin-config SSDT isn't in the repo, so the
  exact GPIO type/pin can't be pinned down without a runtime `_DSM` read or the full
  ACPI table set. Runtime `_DSM` eval / `acpidump` are now possible via sudo.

---

## 5. Next experiments (empirical, root) — hand these to the Dev

Ordered by highest information-per-effort:

1. `sudo i2cdetect -y 2` — confirm OV2680 (0x10) + any PMIC/other on I2C2.
2. Inspect live INT3472 `01`/`02` `/sys`: regulator entries? gpio? clock? (does `intc`
   actually create a rail/clk for the camera now?)
3. Force the media link then STREAMON and observe the *real* failure mode:
   `sudo media-ctl -e ov2680 2-0010:0 -e ipu3-csi2 1:0 -l 'ov2680 2-0010:0,ipu3-csi2 1:0,1'`
   then `v4l2-ctl -d /dev/video1 --stream-mmap`. Distinguish clock-vs-power-vs-link.
4. MCLK path: does ov2680 request a clk? (`grep clk` in dmesg; `/sys/class/clk`).
5. If a real rail/clock is missing → decide **DSDT patch (root, rebuild)** vs
   **userspace `gpio-regulator` + `clk`** fix. (Architect/Dev decision point.)
6. Secondary: `sudo modprobe ov5648`, watch async probe; confirm whether this unit
   physically has a rear cam.

---

## 6. Open questions for a bmad session

- **Architect (Winston):** DSDT patch vs userspace gpio-regulator+clk — what's the
  blast radius on a chezmoi-managed system (custom DSDT = fragile across kernel/BIOS
  updates)? Which is more maintainable / reversible?
- **PM (John):** is "front camera streams" the single MVP, or do we also commit to rear
  OV5648? Do we confirm the hardware actually has a rear sensor first?
- **UX (Sally):** not applicable (kernel driver) — skip.
- **BA (Mary):** not applicable — skip.
- **Dev (Amelia):** can we get a clean capture on video1 once the link is forced and a
  rail/clock is supplied, purely in userspace (no DSDT rebuild)? That's the fastest
  path to proof-of-streaming.

---

## 7. Reference map

- **Live truth:** `_driver-case-live-status.md`, `_resume-todo.txt`.
- **Historical root cause:** `int3472-probe-failure-analysis.md` (Task B, partly stale).
- **Sudoers:** `chezmoi-pi-camera-debug.proposed` (installed), `chezmoi-pi.camerasudo.proposed` (older).
- **Driver source / earlier planning:** `agent_work/archive/chuwi_camera_driver.c`, `day13.md`, `todo.txt`.
- **ACPI:** `DSDT_PATCH_PROPOSAL.md`, `INT3472-linux-analysis.md`, `dsdt.patched.dsl*`.
