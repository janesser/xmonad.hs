# Xmonad → Hyprland on Omarchy: fleet migration strategy

> **Status: AGREED (2026-10-16), execution not started.** WM choice settled:
> Hyprland via Omarchy (the Sway line is dead — see commit "skip sway for
> omarchy/hyprland"; mangowc parked). Fleet strategy agreed with jan
> 2026-10-16. Next: WS1, reconcile the pilot (§5).

* For now trying omarchy (arch linux) with hyprland on it.
  * Haven't found a transition path from current ubuntu installations yet
  * Demands major rework of chezmoi scripts (i.e. apt-get and nala), at least **mise** is already aligned.

* Eventually trying mangowc later. — *parked, no plan.*

## Decisions (2026-10-16)

| # | Question | Decision |
|---|---|---|
| 1 | Fleet end state | **Omarchy where it pays off** — selective, not blanket |
| 2 | Box identities | flowerpower = Chuwi UBook XPro (mobile laptop); no5 = Jetson Nano B210 |
| 3 | peacewagon | Became flowerpower in an uncoordinated "restart from scratch" wipe → Omarchy install |
| 4 | Next box to migrate | **Undecided until maturity is regained** (gate, §7) |
| 5 | xmonad custom features (FloatingVideos, screen corners, mouse gestures, prompts, xmobar) | **All droppable** — no Wayland replacements to build |
| 6 | rpi400 | Stays on Raspberry OS, desktop computing for the kids, out of scope |

## Fleet matrix (end state)

| Box | Role | Base OS | End state |
|---|---|---|---|
| flowerpower (Chuwi UBook XPro) | Mobile workstation | **Omarchy** | Pilot — already migrated; bring under chezmoi control, make it the reference stack |
| lincopta (XMG Core 15, RTX 2060 mobile) | Mobile AI (CUDA llama.cpp) | Ubuntu | Migration candidate, undecided (WS5) |
| cyberkleiber (HP Z6 G4, V100 + GT 730) | AI backend (llama-cuda/sycl handover) | Ubuntu | Migration candidate, undecided (WS5); AI-stack stability likely wins |
| no5 (Jetson Nano B210) | CUDA/TensorRT LLM box | L4T 4.9 (Ubuntu-based) | **Frozen** — NVIDIA support ceiling, no distro-upgrade path, ever |
| rpi400 | Kids' desktop | Raspberry OS | **Frozen** — minimal management, no desktop-stack migration |
| Aspire E5-573G | — | Win11 | Out of scope |
| FOSS Android | Phones | e/OS & co. | Out of scope (own track) |

## Principles

1. **The repo is the single source of truth** for every non-frozen box. Anything
   that lives on a box but not in the repo is a defect, not a feature.
2. **Roles, not boxes.** Chezmoi tree = shared base + role overlays; boxes declare
   a role, per-host overrides stay minimal (the `.chezmoi.toml.tmpl` hostname
   toggle pattern continues to work).
3. **Frozen boxes are a first-class role** (no5/Jetson, rpi400). Their scripts keep
   working on their bases; nothing migrates, nothing rots.
4. **Migration is a scripted step.** After the pilot matures, moving a box =
   install base OS → add age recipient → `chezmoi init` + `cz apply`. No hand-work.
5. **Muscle memory is the only desktop requirement.** The xmonad keymap port
   (`~/.config/hypr/hypr-xmonad.lua`, to be captured into the repo) is the entire
   "port"; everything else comes from Omarchy defaults.

## The gap: the pilot is ahead of the repo

The flowerpower restart happened **outside** chezmoi. Unmanaged today:

- `~/.config/hypr/` — hyprland.lua, bindings.lua, input.lua, monitors.lua,
  looknfeel.lua, autostart.lua, **hypr-xmonad.lua** (the xmonad keymap port),
  hyprsunset.conf, xdph.conf
- `~/.config/omarchy/` — shell.toml/json, themes, hooks, plugins, extensions,
  defaults, branding
- **No age recipient for flowerpower** → the 44 encrypted files (ssh config,
  gitconfig, evolution, pi-agent settings, keepassxc, …) cannot be decrypted here
- The scoped sudo drop-in for pi-agent (`/etc/sudoers.d/chezmoi-pi`) is
  apt/nala-based and not present → the "pi-agent may run `cz update`" property
  does not hold on the pilot (verify in WS1; `/etc/sudoers.d` not listable as jan)
- Stale artifacts of the old world: `peacewagon` age recipient,
  `xmonad/xmobarrc.peacewagon`

## Workstreams

**WS1 — Reconcile the pilot (flowerpower).** Capture the current local hypr/omarchy
config into the repo (chezmoi files, not symlinks), add the flowerpower age
recipient + re-encrypt, re-establish the scoped sudo drop-in (pacman flavour),
and verify a clean `cz apply` converges the box. flowerpower becomes the
reference stack.

**WS2 — Role catalog + repo restructure.** Define roles (workstation-mobile,
ai-server, frozen-special, …), tag every run script by role, and make the
Ubuntu-only scripts (apt/nala/snap/extrepo) unreachable from Omarchy boxes —
rewrite or retire. The proven mechanisms stay: hash-gated `run_onchange`, age
encryption, `.chezmoi.toml.tmpl` toggles, run_once/run_onchange discipline.

**WS3 — Desktop-stack retirement.** Drop the xmonad/xmobar/lightdm/X11-lock
subtrees (xmonad.hs + lib/, xmobar + 3 host variants, xautolock/slock/xss-lock,
lightdm fixups, the Chuwi xkb workaround) — except: verify the Chuwi pipe-key
combo still works under Wayland (open item). `hypr-xmonad.lua` is the only
remaining desktop delta from Omarchy defaults.

**WS4 — Frozen-box hygiene.** Confirm the Jetson (no5) + RPi scripts still apply
cleanly on their bases; document the frozen status in `devices/` so nobody
"upgrades" them later.

**WS5 — Next box (gated by §7).** Pick between lincopta (mobile + CUDA — the
interesting one) and cyberkleiber (AI backend — likely stays Ubuntu). Decide
with fresh eyes once the pilot is mature.

## Maturity gate — "maturity regained" means

- [ ] flowerpower converges from a clean Omarchy install via `chezmoi init` +
      `cz apply` (reproducibility test)
- [ ] all desktop config (hypr + omarchy) is chezmoi-managed; `cz diff` quiet at rest
- [ ] all 44 encrypted files decrypt on flowerpower; recipients current
      (flowerpower added, peacewagon disposition decided)
- [ ] scoped sudo drop-in installed; pi-agent runs `cz update` unattended
- [ ] role gates in place: an Omarchy box never runs an apt script (and vice versa)
- [ ] xmonad subtree retired or clearly marked legacy
- [ ] `devices/` documents the frozen boxes (no5/Jetson, rpi400) and their bases

## Risks / open items

- **Omarchy churn** — custom distro, fast-moving (kernel 7.2.5-3-omarchy,
  omarchy-shell). Accept churn or pin versions; decide during WS1.
- **Chuwi pipe-key combo** was an X11 xkb symbols hack; needs verification under
  Wayland/libinput (WS3).
- **cyberkleiber handover protocol** is systemd-level (portable to Arch), but
  its provisioning scripts are apt-level (WS2 rewrite).
- **Patched btop** (cap_perfmon, Intel-GPU fix) is a user-local binary —
  portable; its run script needs a role gate.
- **peacewagon age key** — keep the recipient until confirmed no other box uses
  it, then retire.
