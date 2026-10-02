# Graphics cards (HP Z6 G4)

Host: `cyberkleiber` · kernel `7.0.0-38-generic` · Ubuntu 26.04.1
Goal: **GT 730 = display**, **V100 = compute**. — **RESOLVED (2026-10-03):** GT 730 on `nouveau`, V100 on `nvidia` 580, coexisting.

## Hardware

| GPU | PCI | Arch | CUDA | Role |
|---|---|---|---|---|
| Intel DG1 / Iris Xe | `2f:00.0` | — | no | primary display (i915, GNOME/Wayland) |
| NVIDIA Tesla V100 SXM2 32G | `21:00.0` | Volta GV100 | yes | compute (headless, no CRTC) |
| NVIDIA GeForce GT 730 | `17:00.0` | Maxwell GK208B (10de:1287) | yes | **display (nouveau)** |

## Current state (resolved)

- `nvidia-driver-580` (580.178.04) installed and loaded. Drives the **V100**;
  `nvidia-smi -L` reports it. (Boot-time log above still shows the 580 ignoring
  the GT 730 — that is the *pre-nouveau* state.)
- The **GT 730 is now driven by `nouveau` 1.4.2** — bound to `17:00.0`, minor 3,
  2 GiB VRAM, connector connected:

  ```
  nouveau 0000:17:00.0: NVIDIA GK208B (b06070b1)
  nouveau 0000:17:00.0: fb: 2048 MiB GDDR5
  [drm] Initialized nouveau 1.4.2 for 0000:17:00.0 on minor 3
  ```

- The two drivers **coexist**: loading `nouveau` while `nvidia` holds the V100
  leaves the V100 bound to `nvidia` (EBUSY) and only binds the free GT 730. No
  need to blacklist `nvidia` or touch initramfs.
- Iris Xe stays on `i915`.

## Root cause

NVIDIA classifies the **GK208B / GM108 Maxwell** as a *legacy* GPU. Only the
**470.xx legacy branch** supports it; every current branch (525/535/550/560/570/580)
explicitly ignores it. So there is **no newer proprietary driver** that makes the
GT 730 work — 470.xx *is* the newest that supports it.

## Why not the 470.xx legacy driver (option A — abandoned)

470.xx would be the "one driver for both cards" fit, but:

- Building 470.256.02 against kernel 7.0 (attempted 2026-10-01) failed with
  kernel-7.0 ABI breakage — ~130 errors (`get_user_pages` long-form sig,
  `mmap_lock`, `timespec64`, `f_path.dentry`, `__kuid_val`, conftest sigs, …).
- Building 470 means **replacing** the working 580 that drives the V100 + CUDA.
  So 470 would only be in play to gain CUDA/NVENC on the GT 730 *display*
  — rejected in favour of keeping 580 on the V100 and adding `nouveau` for the
  display.

## Resolution — nouveau + `mxm_wmi` (deployed 2026-10-03)

`nouveau` drives the GK208B out of the box on kernel 7.x (no build needed, no
CUDA/NVENC — it is a display-only driver), but it has **one hard dependency**:
the `mxm_wmi` module, which exports `mxm_wmi_supported`, `mxm_wmi_call_mxmx`
and `mxm_wmi_call_mxds`. Without `mxm_wmi` loaded first, `nouveau` fails at load:

```
nouveau: Unknown symbol mxm_wmi_supported (err -2)
```

(`modprobe` is *supposed* to pull that dependency automatically — see the anomaly
below; it does not on this host.)

### Anomaly: `modprobe nouveau` mis-resolves on this host
`modprobe nouveau` reports `could not find module by name='off'`, and
`modprobe -n -v nouveau` returns empty rc0. Workaround: **insmod by absolute
path** — the deployed wrapper does this.

### Durable config (chezmoi, commit `fdeeadc`)

- `/usr/local/bin/load-nouveau-gt730.sh` — idempotently insmods
  `mxm-wmi.ko.zst` then `nouveau.ko.zst` by path (skips if already loaded).
- `/etc/systemd/system/nouveau-gt730.service` — `Type=oneshot`,
  `After`/`Wants=sys-module-nvidia.device`, so the V100 is owned by `nvidia`
  *before* `nouveau` loads — it can never steal the V100 at boot.
- Installed by the hardware-guarded run script
  `.chezmoiscripts/run_once_5_aitools_6nouveau_gt730_driver.sh`, which deploys
  **only when both a GT 730 (`10de:1287`) and a V100 (`10de:1db5`) are present**
  (gpu.func helpers `has_nvidia_gt730` / `has_v100`, using `lspci -nn`).
- The temporary passwordless `modprobe`/`insmod` sudoers drop-in used during
  testing (`/etc/sudoers.d/10-temp-nouveau-test`) is removed on every apply by
  `.chezmoiscripts/run_9_cleanup_nouveau_temp_sudoers.sh`.

### Open item

The kernel/module layer is verified. The **display layer** — the desktop
actually rendering on the GT 730 monitor under GNOME — is confirmed after a
reboot; the current session predates `nouveau`.
