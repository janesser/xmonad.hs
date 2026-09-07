# Driver case — live status (2026-09-07, freshly booted Chuwi UBook XPro)

> Updated after resuming. This supersedes the "fix/proc_thermal_release_ip3.sh"
> experiment as the immediate next step — see below.

## Verified current state (kernel 6.8.0-139-generic)

**The stack is already mostly working — much further than the analysis doc assumed.**

- `ipu3-imgu` binds the Imaging Unit at `00:05.0` ([8086:1919]) — the modern imgu driver,
  loads firmware, `ipu_bridge` up.
- `ipu3-cio2` binds the CSI-2 host at `00:14.3` ([8086:9d32]) and reports
  `Found supported sensor OVTI2680:00` / `Connected 1 cameras`.
- **Front camera OV2680 (CAM0) is bound**: `/sys/bus/i2c/devices/i2c-OVTI2680:00/driver -> ov2680`,
  and it read `sensor_revision id = 0x2680`.
- Discrete PMIC driver **already loaded**: `intel_skl_int3472_discrete` (the `intc` the
  analysis recommended) + `tps68470_regulator`/`clk_tps68470`. `regulator.0` exists but is a
  **platform `reg-dummy`** — not a real power rail. ov2680 logged
  `supply DOVDD/DVDD/AVDD not found, using dummy regulator`.
- 16 `/dev/video*` (media0 = cio2 → video0-3; media1 = ipu3-imgu → video4-13).

## Remaining issues

1. **Front cam binds but won't stream.** `v4l2-ctl -d /dev/video1 --stream-mmap` →
   `VIDIOC_STREAMON returned -1 (Link has been severed)`. Media graph shows
   `ov2680 2-0010:0 -> ipu3-csi2 1:0` link present but **disabled** (no [ENABLED]).
   Likely cause: discrete PMIC rails are dummy → sensor can't actually power up for CSI.
2. **Rear camera OV5648 (CAM1) not loaded.** `i2c-OVTI5648:00` exists (ACPI-enumerated on
   I2C2, addr 0x36) with modalias `acpi:OVTI5648:OVTI5648:`, but `ov5648` module is NOT
   loaded this boot and there are zero dmesg lines for it. (Module exists:
   `/lib/modules/$(uname -r)/kernel/drivers/media/i2c/ov5648.ko.zst`.) Whether this unit
   physically has a rear camera is unconfirmed.

## The old fix script is now likely obsolete

`fix/proc_thermal_release_ip3.sh` was built on the premise "IP3 at `00:04` ([8086:1903])
is grabbed by `proc_thermal`, so `ipu3-cio2` never binds". On THIS kernel the imaging
pipeline uses the modern `ipu3-imgu` (00:05) + `ipu3-cio2` (00:14.3) split and **both bind
cleanly at boot**. The IP3@00:04 / proc_thermal story does not apply here. **Do not run
that experiment** without first re-checking against the live layout.

## Blocker

- **No passwordless sudo.** PCI rebind, `modprobe ov5648`, `i2cdetect`, enabling media
  links, and capturing if `video`-group perms are restricted all need root. `sudo` here
  asks for a password (scoped `chezmoi-pi` drop-in only covers chezmoi run scripts).
  Need a sudo password (or a targeted NOPASSWD rule) to do capture/PMIC verification.

## Updated diagnosis (2026-09-07, session 2): why the front cam can't stream

Confirmed `intc` binds `INT3472:01`/`:02` (status=15, enabled) but creates **no** regulator
and **no** Mclk for the camera.

Root-cause chain:
- `int3472-discrete` registers a real power rail (`skl_int3472_register_regulator`) / GPIO
  clock ONLY when an INT3472 PMIC's `_CRS` GPIO has a `_DSM` (GUID `79234640-…`) entry whose
  packed type field (bits 7:0) == `POWER_ENABLE` (0x0b) or `CLK_ENABLE` (0x0c). The map only
  exposes supplies `"avdd"`/`"AVDD"` (clk_and_regulator.c).
- The DSDT INT3472 `PMIC-CRDG` device exposes `_CRS` GPIOs built from `PINR(C0P#, C0G#)` and its
  `_DSM` returns `GPPI(C0F0, (0x18*C0G#)+C0P#, C0I#, C0A#)`. `_STA` is `0x0F` only if
  `CL00 && C0TP==1`, else the device is hidden.
- The camera-serving controller is `INT3471` (`CAM0`, `_HID INT3471`, `_DEP` on one PMIC) and
  `INT3474` (`CAM1`). NOTE: their `_DDN` still read `IMX135-CRDG2` / `OV2740-CRDG2` — the DSDT
  is the raw firmware with the OLD sensor names (matches the analysis note that the DSDT is
  unpatched).
- **The `C0*` pin VALUES are declared `External` in the DSDT but defined in a power-config
  SSDT that is NOT in the acpidump folder** (grep found no `Name (C0GP,…)`, only External
  decls). So the actual GPIO pin/type cannot be resolved statically from what we have.

Conclusion: the camera-serving INT3472 either isn't the enabled one, or its `_DSM`/`GPPI`
convention doesn't surface a `POWER_ENABLE`/`CLK_ENABLE` pin, so `intc` never creates the rail
→ OV2680 stays on `reg-dummy`, gets no Mclk, and the `ov2680 -> ipu3-csi2` media link stays
disabled → STREAMON "Link severed". (Dummy rails + no clock = no streaming even if the link
were forced on.)

## Blockers (real)

- **No sudo** → can't evaluate the INT3472 `_DSM` at runtime, rebind, patch the DSDT, add a
  userspace `gpio-regulator`/`clk`, or verify a capture. Every concrete next step needs root.
- The `C0*` pin config SSDT isn't in the repo, so static resolution of the GPIO type stalls
  without the full ACPI table set (or a runtime `_DSM` read).

## Suggested next steps (awaiting decision)

A. Get a sudo password / targeted NOPASSWD rule — this unblocks everything (runtime `_DSM`
   eval, capture verify).
B. With sudo: dump the missing power-config SSDT + read the INT3472 `_DSM` to see the exact
   GPIO type/interrupt/pin fields; decide DSDT patch vs. userspace gpio-regulator+clk fix.
C. Secondary: confirm whether this unit physically has a rear `OV5648` (load `ov5648`, watch
   async probe).
