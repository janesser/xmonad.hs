# Jetson Nano (B210) — install guide

Tegra **X1** (`tegra210`). This box is set up for **CUDA / TensorRT** so we can run
small LLMs with `llama.cpp`. That choice fixes everything below, so read the one
constraint first — it kills two of the things you originally asked for.

## The one constraint (accept this or change direction)

CUDA on the Nano lives **only** in NVIDIA's proprietary **L4T / JetPack 4.6.6**,
kernel **`4.9.337-tegra`**. CUDA 10.2 is the last version Tegra X1 will ever get,
and it is not available on any mainline 6.x kernel. Consequence:

- **No recent kernel.** Stuck on `4.9.337-tegra`.
- **No normal distro-upgrade cycle.** Fixed L4T image; you modernize *userland*,
  you don't `do-release-upgrade` the base.

If you don't actually need CUDA, drop it and see `README.md` for the Armbian /
OE4T-Yocto / recent-kernel routes. If you do need CUDA — which you do, for LLMs —
this is the only path, and it's fine.

## Which image

**Recommended — mischa-robots `jetson-nano-ubuntu22`**
L4T R32.7.6, kernel `4.9.337-tegra`, **CUDA 10.2.300**, cuDNN 8.2.1, TensorRT
8.2.0.1, OpenCV 4.8 (CUDA), DeepStream 6.0, Python 3.10 — on a clean **Ubuntu
22.04** userland (vs. 18.04 on the stock image). Includes `jtop` + fan control.
→ <https://github.com/mischa-robots/jetson-nano-ubuntu22>

> ⚠️ **Private beta — no public download link yet.** Until they publish one, use
> the **stock NVIDIA JP4.6.1 SD image** below (same L4T R32.7.6 / CUDA 10.2, just
> Ubuntu 18.04 base). It's the proven baseline.

## Prerequisites

- Host PC with **Balena Etcher** (Windows/macOS/Linux) or `dd`.
- **micro-USB (USB-B) → USB-A** cable for serial console / recovery.
- **SD card**: Class 10, **32–64 GB** (64 GB recommended). A slow/old card is the
  #1 cause of "it won't boot."
- First boot: a **display + USB keyboard**, or an **ethernet** cable.
- Optional: USB stick for a swapfile / high-IO files (reduces SD wear).

## 1. Flash the SD card

### mischa-robots (once released)

```bash
xz -dc jetson-nano-ubuntu22-server.img.xz | sudo dd of=/dev/sdX bs=4M status=progress
sync
```

### Stock NVIDIA JP4.6.1 (proven baseline)

```bash
unzip -p ~/Downloads/jetson-nano-jp461-sd-card-image.zip \
    sd-blob-b01.img | sudo dd bs=8M of=/dev/mmcblk1 status=progress oflag=sync
```

The `sd-blob` write is what sets the cboot partition layout the Nano's bootloader
needs. (The `461` in the filename is just a version marker — it means nothing.)

## 2. First boot

- Insert the SD card, power on.
- The rootfs partition **auto-expands** to fill the card (~30 s), then it's ready.
- **mischa-robots** login: `jetson` / `jetson`.
- **Stock JP4.6.1** login: `ubuntu` / `ubuntu`.
- Change the password (`passwd`), ensure the user has sudo, set a hostname.

## 3. Network

```bash
# ethernet: just plug it in — connects automatically
nmcli device wifi list
sudo nmcli device wifi connect "YourSSID" --ask
# then SSH in:
ssh jetson@<hostname>.local
```

## 4. Verify the CUDA stack

```bash
nvcc --version
nvidia-smi
sudo jtop            # CPU / GPU / RAM / temp / fan
```

If `nvcc` isn't found in an interactive shell, put CUDA on PATH:

```bash
sudo tee /etc/profile.d/cuda.sh >/dev/null <<'EOF'
export PATH=/usr/local/cuda/bin${PATH:+:${PATH}}
export LD_LIBRARY_PATH=/usr/local/cuda/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
EOF
source /etc/profile.d/cuda.sh
```

`mischa-robots` ships `sudo ~/test-sdk.sh` for full L4T validation.

## 5. Run a small LLM with `llama.cpp` (+ CUDA)

Easiest — prebuilt for nvcc 8.5:

```bash
curl -fsSL https://kreier.github.io/llama.cpp-jetson.nano/install.sh | bash && source ~/.bashrc
llama-cli -hf ggml-org/gemma-3-1b-it-GGUF --n-gpu-layers 99
```

First load takes **~6–7 min** (model compile), later runs ~10 s. Or serve it and
open the web UI at `http://<jetson>:8080`:

```bash
llama-server -m ~/.cache/llama.cpp/ggml-org_gemma-3-1b-it-GGUF_gemma-3-1b-it-Q4_K_M.gguf \
    --host 0.0.0.0 --n-gpu-layers 99
```

A gcc 9.4 variant exists (`install9.sh`) if the 8.5 build misbehaves on your
system; source is at <https://github.com/kreier/llama.cpp-jetson>.

## 6. Deploy dotfiles via chezmoi

`cz apply` installs the configs bundled here in `etc/` — see `README.md`.

> ⚠️ **Not yet verified** against the L4T R32.7.6 image. These configs were written
> against an earlier, unknown base and will probably need tweaking (e.g. lightdm's
> `[SeatDefaults]` → `[Seat:seat0]`). Inspect and fix after applying.

- `etc/modprobe.d/tegra-udrm.conf` — DRM/KMS (`tegra_udrm modeset=1`), needed for
  Wayland / a proper display server.
- `etc/lightdm/…` + `etc/X11/default-display-manager` — pick your window manager
  at the lightdm login screen.

## Expected performance & limits

- **4 GB shared LPDDR4** (~14 GB/s bandwidth), CUDA 10.2 on a small Pascal/Maxwell
  GPU. This is a play/learn box, not an inference server.
- **Comfortable:** 1–3 B models (e.g. Gemma-3-1B, `Q4_K_M`) with most layers on the
  GPU, modest tokens/sec.
- **Pushed:** 8 B via near-1-bit Bonsai quant (<https://github.com/coverblew/llamita.cpp>,
  ~1.1 GB) — slow, token-by-token.
- **Known gaps:** no Vulkan on the Nano; TensorRT/VPI Python bindings are
  non-functional (C++ works); newer JetPack 5.x libraries won't build on 4.9.

## Troubleshooting

| Symptom | Fix |
| --- | --- |
| Won't boot / stuck on NVIDIA logo | Fast, good-capacity SD card; reflash the `sd-blob`; try the stock image first. |
| `nvcc: command not found` | Set the CUDA PATH (step 4). |
| CUDA OOM on a bigger model | Lower `--n-gpu-layers`, use a smaller / harder-quant model, or offload layers to CPU. |
| `eglinfo` / display issues | Ensure `tegra_udrm` is loaded (`sudo modprobe tegra-udrm modeset=1`); see `README.md`. |
| Recent kernels / Armbian / OE4T-Yocto / `do-release-upgrade` | Not this path — see `README.md`. |

## Sources

- mischa-robots `jetson-nano-ubuntu22` — <https://github.com/mischa-robots/jetson-nano-ubuntu22>
- `llama.cpp` CUDA build — <https://github.com/kreier/llama.cpp-jetson.nano>
- Stock L4T R32.7.6 / JetPack 4.6.6 — <https://developer.nvidia.com/embedded/jetson-linux-archive>
