# jetson_nano_b210

NVIDIA Jetson Nano, model **B210** (4 GB), Tegra **X1** (`tegra210`). NVIDIA
dropped Jetson Nano support after **Jetson Linux R32.7.6** (JetPack 4.6.6).

This box was configured for **CUDA / TensorRT** so we can run small LLMs with
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
- **Armbian (vanilla kernel).** Re-added to Armbian builds Oct 2026 under the
  `uefi-arm64-dt` target with the current edge kernel (7.3); community nightly
  images ship regularly. Boots after a QSPI-bootloader reflash + a `venc` DTB
  patch; no NVIDIA GPU/CUDA (pure arm64 CPU). The only way to run a fully
  patched, modern Linux on this hardware.
- **OE4T / Yocto `meta-tegra` — deferred.** Reproducible L4T images; same CUDA
  ceiling. Your kirkstone build produced a fine SD image that won't survive an
  upgrade cycle — by design. → <https://github.com/janesser/kirkstone-jetson-nano>
- **Orin Nano (hardware replacement).** NVIDIA's actual successor, supported to
  ~2032, with full JetPack 6/7 + L4T R36+/R39 and modern kernels. The only way
  to keep the CUDA pipeline while getting security patches.

## Status: abandoned (2026-11-05)

This box's security story ended when NVIDIA released **L4T R32.7.6** as the last
Tegra X1 release — in November 2024. The kernel (`4.9.337-tegra`) has received no
security fixes since and will never receive any. Ubuntu 18.04 userland is also EOL
(Canonical ESM covers standard packages only — not NVIDIA's L4T-modified kernel or
its proprietary modules). In practice:

- **The box cannot be patched at the kernel level.** No upstream kernel, no CVE
  fixes for the display/audio/network/TEE stack. This is a permanent state.
- **Running it networked without air-gapping is an unacceptably high risk.**
  There is no maintained alternative — not Ubuntu Pro (it doesn't cover L4T's
  NVIDIA kernel), not Armbian on top of L4T (same kernel), and nothing that can
  be reverse-engineered (NVIDIA IP, no public docs, no open-source base driver).
- **The community keeps the userspace drivers alive** — `L4T-Community` / `L4T
  Fedora` on Copr maintains NVIDIA's proprietary stack for Fedora on Tegra X1,
  but it still boots from an R32.x-era kernel, so it doesn't close the kernel-level
  gap. It preserves *userspace* — not the security story.
- **The Tegra X1 GPU is unusable outside L4T.** Without NVIDIA's proprietary stack
  there is no CUDA, no TensorRT, no OpenGL ES, no hardware video decode — it
  becomes a Raspberry-Pi-class arm64 box with all of its advantages gone.

**Decision:** the hardware is being abandoned. There is no way to keep both CUDA
and security on Tegra X1. The successors that do:

- **Armbian `uefi-arm64-dt`** (re-added Oct 2026) — full modern stack, no GPU.
- **Jetson Orin Nano (T254)** — NVIDIA's actual successor, supported ~2032, with
  JetPack 6/7 + L4T R36+/R39 and modern kernels. The only path to keep the CUDA
  pipeline while getting security patches.

Everything below is kept as a record of how it was configured, not as guidance for
future deployment.

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
