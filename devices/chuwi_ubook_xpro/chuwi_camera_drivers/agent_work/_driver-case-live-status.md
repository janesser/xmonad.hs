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

---

## Update (2026-09-08, session 3 — sudo now installed, option A investigation)

**Blocker cleared:** scoped NOPASSWD camera-debug sudo is installed
(`/etc/sudoers.d/chezmoi-pi-camera-debug`); passwordless `sudo -n` confirmed for the 10
camera tools. The `chezmoi-pi` rule was left untouched.

### Empirical results (kernel 6.8.0-139-generic)

- **I2C bus 2 has ZERO responding devices.** `sudo i2cdetect -y 2` shows no ACK at `0x10`
  (OV2680) or `0x4c` (TPS68470 PMIC); even `i2cget 0x4c 0x00` fails. `-- --` = no ACK.
  → The PMIC + sensor are **unpowered**, so they can't be probed. A2 "live hardware probe"
  is blocked by a chicken-and-egg (unpowered because of the very bug we're fixing).
- `int3472-discrete` IS bound to `INT3472:01`/`:02` (STA=15), but those are the GPIO
  expander instances — NOT the camera PMIC.
- The actual PMIC `\_SB.PCI0.I2C2.PMIC` (`INT3472`, `_UID=0`, "PMIC-CRDG2", `_ADR=0`) is
  **NOT bound**: its `/sys/.../INT3472:00/status` = **0**, so it is **hidden** in ACPI.
- `tps68470_regulator` / `clk_tps68470` are loaded with **0 references** (modprobe'd during
  debugging, never bound). No `/sys/class/clk/` exists → **no MCLK**.
- ov2680 still on `regulator-dummy` → AVDD/DOVDD/DVDD not powered.

### Root cause, now precisely pinned

The `I2C2.PMIC` device is **self-contained in the DSDT** — everything is defined **except one
object**:

```aml
Device (PMIC) { _ADR=0, _HID=INT3472, _UID=0, _DDN=PMIC-CRDG2
  _CRS: I2cSerialBusV2(0x004C, ..., "\\_SB.PCI0.I2C2")   // fully defined ✓
  CLDB: 0x00,0x02,...                                    // control_logic_type = 2 (TPS68470) ✓
  _DSM: UUID 26257549-9271-4ca4-bb43-c4899d5a4881;
        Arg2==2 -> 0x02004C0B   (low byte 0x0B = POWER_ENABLE) ✓
  _STA: If ((SCSS == One)) Return(0x0F) else Return(0)    // gated on SCSS
}
```

`SCSS` is declared **`External`** (DSDT line 1369) and **never assigned**, so `_STA=0` →
the PMIC is hidden → no driver binds → OV2680 dummy rails + no MCLK + disabled media link →
STREAMON "Link severed". This is far narrower than the day-7/14 "GPIO pin reconstruction"
hypothesis.

**Windows ground-truth scan** (`20260819_dmesg_ipu3`, dump_intel_ipu_data from the Windows
boot) confirms the live values:
- `control_logic_type: 1` (DISCRETE/CRD-D) for the two INT3472 instances
- `mclk_speed: 19200000` (19.2 MHz), `mclk_port: 0`
- `sensor_card_sku: 32`

### Two caveats that reshape option A

1. **`SCSS` is likely EC-gated, not a missing SSDT.** If the Windows `SkcController`
   driver drives the embedded controller to enable camera power, Linux has no equivalent →
   `SCSS` stays 0 even with a correct DSDT. The real fix may then be an **EC /
   camera-controller driver**, not an ACPI patch. (`SkcController.sys` is in the Windows
   driver kit: `UBook XPro Drivers/System devices/skccontroller.inf_amd64_.../`.)
2. **Board data is vendor-specific.** Even after `SCSS==1`, the TPS68470 MFD driver needs
   Chuwi's board data to map its output rails → AVDD/DOVDD/DVDD and the MCLK output. Not in
   the kernel or the Windows kit as a parseable file.

### Decision: recover the ground-truth power config by dumping from Windows

Live recovery under Linux is impossible (PMIC unpowered). Windows is **dual-boot installed**
(`Boot0003 Windows Boot Manager`, `/dev/sda*`), and its cameras work — so it has the live
`SCSS=1` power config. Plan:

1. **Boot Windows** (fully reversible).
2. Dump the ACPI tables there (e.g. `acpidump` in Windows, or ACPI-Studio / export) → the
   real power-config SSDT with `SCSS=1` and the defined `C0*` values.
3. Run the same `dump_intel_ipu_data` tool → TPS68470 board data + which pin is MCLK.
4. With ground truth: decide between (A) patch the Linux DSDT to make the `I2C2.PMIC`
   visible (`SCSS==1`) + supply board data, or (B) add an EC/camera-controller driver so
   `SCSS` gets set on Linux.

**⚠ Reboot into Windows needs explicit user go-ahead** (boot-level, daily-driver chezmoi box).
Prep a safe ACPI-dump procedure before rebooting. Nothing Linux-side is changed by booting
Windows.
