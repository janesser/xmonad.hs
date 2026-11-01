## btop: dual GPU (Iiris Xe + NVIDIA) visibility

btop only shows the NVIDIA box for the Intel Iris Xe / DG1 (gpu1). This is a
**btop bug fixed by patching + rebuilding btop**, not a config issue.

- **Deployed:** `~/.local/bin/btop` = `1.4.7+2ad5308`, `cap_perfmon=ep`, `jan:jan`
  — the patched binary; `/usr/bin/btop` stays the vanilla apt package.
- **`CAP_PERFMON` also required:** `sudo setcap cap_perfmon+ep ~/.local/bin/btop`
  (needs jan's sudo **password** — the NOPASSWD drop-in only names `/usr/bin/btop`)
- **Deploy script:** `.chezmoiscripts/run_once_5_aitools_3btop_intel_gpu_cap.sh`
  (run_once, tracked in git). **Policy: patched btop only where an Intel GPU
  exists; every other host keeps the vanilla system btop untouched.**
- **Dev clone:** `~/projs/btop` (branch `btop-intel-gpu-fix`), deploy uses a
  separate clean clone at `~/projs/btop-intel-gpu`.
- **Config:** `~/.config/btop/btop.conf` (`shown_gpus = "nvidia amd intel"`,
  `shown_boxes = "cpu mem net proc gpu0 gpu1"`).
- **Verification (headless):** run with `--debug` and confirm the log has **no**
  `Failed to find Intel GPU engines` and **no** `Failed to initialize PMU`.
- **Root cause / patch details** are in `discoveries/` — not needed for daily
  work.

