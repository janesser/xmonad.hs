# jetson_nano_b210

NVIDIA Jetson Nano, model **B210** (4 GB), Tegra **X1** (`tegra210`). NVIDIA
dropped Jetson Nano support after **Jetson Linux R32.7.6** (JetPack 4.6.6).

This box is configured for **CUDA / TensorRT** so we can run small LLMs with
`llama.cpp`. See **`INSTALL.md`** for the authoritative, step-by-step setup —
image choice, flashing, first boot, CUDA verification, and running a model. Read
it before anything else.

## The one thing to understand

CUDA here is **proprietary and pinned to L4T `4.9.337-tegra`** — it will never
move to a newer kernel. So this box has **no recent kernel and no normal
distro-upgrade cycle**; you modernize userland, you don't `do-release-upgrade`.
If CUDA isn't required, the other routes live below.

## Options (and their status)

- **L4T / JetPack 4.6.6 image — the CUDA path. ✅ Recommended.** Stock JP4.6.1
  (Ubuntu 18.04, proven) or the **mischa-robots `ubuntu22`** image (same L4T
  R32.7.6 / CUDA 10.2, but on Ubuntu 22.04 — in private beta, no public download
  yet). Full CUDA/cuDNN/TensorRT + `llama.cpp` support. → `INSTALL.md`
- **Armbian (vanilla kernel) — deferred.** Boots only after a QSPI-bootloader
  reflash + a `venc` DTB patch; no NVIDIA GPU/CUDA (pure arm64 CPU), and Armbian
  no longer maintains this board. Interesting only if you *drop* CUDA.
- **OE4T / Yocto `meta-tegra` — deferred.** Reproducible L4T images; same CUDA
  ceiling. Your kirkstone build produced a fine SD image that won't survive an
  upgrade cycle — by design. → <https://github.com/janesser/kirkstone-jetson-nano>

## Config files — pending verification

The bundled `etc/` configs (`modprobe.d/tegra-udrm.conf`, `lightdm/*`,
`X11/default-display-manager`, `modules`) have **not** been verified against the
chosen OS image (L4T R32.7.6 / JetPack 4.6.6, Ubuntu 18.04 or 22.04). They were
written against an unknown earlier base and will likely need adjustment — e.g.
lightdm's `[SeatDefaults]` section is stale (should be `[Seat:seat0]` on newer
lightdm). Treat `cz apply` here as a starting point to inspect and fix, not as
known-good.

## Still-applicable notes (L4T 4.9 environment)

- **Fallback kernel** — if you ever rebuild the L4T kernel, the `nvidia-l4t-kernel`
  packages clobber `/boot/Image`. Keep a working `4.9` kernel bootable (see git
  history for the extlinux primary/backup setup).
- **jtop** — `sudo pip install -U jetson-stats && sudo jtop --install-service`;
  handy for clocks / nvpmodel power modes.
- **podman on 4.9** — overlayfs needs ≥ 4.12, so use the `vfs` storage driver
  (`/usr/share/containers/storage.conf`, `driver = "vfs"`). NVIDIA CDI:
  `sudo nvidia-ctk cdi generate --mode csv --output=/var/run/cdi/nvidia.yaml`.
- **Window manager** — `cz apply` sets up lightdm (`etc/lightdm`,
  `etc/X11/default-display-manager`); pick your WM at login.
- **DRM/KMS (Wayland)** — load `sudo modprobe tegra-udrm modeset=1`; the packaged
  `etc/modprobe.d/tegra-udrm.conf` enables it. Without it `eglinfo` reports no
  display and Wayland can't start.

## Sources

- <https://developer.nvidia.com/embedded/jetson-linux-archive>
- <https://developer.nvidia.com/embedded/linux-tegra-r3276>
- <https://github.com/mischa-robots/jetson-nano-ubuntu22>
- <https://github.com/kreier/llama.cpp-jetson.nano>
