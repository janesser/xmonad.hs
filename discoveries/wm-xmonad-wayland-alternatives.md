# Xmonad → Wayland alternatives: which one lets me keep my keybindings?

Date: 2026-08
Context: migrating jan's personal xmonad setup to a Wayland compositor.

## Quick answer

No Wayland WM lets you keep `xmonad.hs` verbatim — the keybinding *system* is
different everywhere. But there's a clear winner for "keep most of my
keybindings", then a tiered rest:

1. **xmonad-wayland** (cristiancmoises) — keep the same `xmonad.hs`, same keybindings. Best match. Caveat: no xmonad-contrib, no X11 hooks, no manageHook — and *your* config is heavily dependent on all three, so this needs the most porting of the "easy" candidates despite the identical key names.
2. **Sway** — i3-style config file; most of your `$mod` combos port directly. Rewrite, not a port. Also the most battle-tested to maintain long-term.
3. Hyprland / Niri / Qile / River — rewrite, flexible.
4. Råu — Emacs-managed Wayland (you already live in Emacs for org/journal).

The deciding factor is not the key names but which parts of your config you can
part with: **xmonad-contrib actions, `manageHook` className rules, and X11-only
hooks (xcompmgr/xprop/xkill/xautolock).**

## Your current config — the things that survive the port

From `dot_config/xmonad/xmonad.hs`:

- **modMask = mod4 (left Super)** — standard, preserved everywhere.
- **Keybinding style:** grouped, *descriptive* named keymaps (`myBasicKeys`,
  `myWindowKeys`, `myJournalKeys`) via `mkNamedKeymap`/`addDescrKeys`. Pure
  presentation — becomes comments in any file-based config.
- **Non-ASCII keys:** `ö`, `ä`, `ü` (e.g. `C-ö` → copyq toggle). The single
  trickiest thing to port to Wayland — see "gotchas".
- **Leader chord:** `M-o S-j` / `S-k` / `S-t` … (leader = `Super+o`, then
  `Shift+<letter>`). Needs a multi-key leader/`mode`; Sway and Hyprland both
  support it.
- **xmonad-contrib actions you'd lose verbatim:** `ToggleStruts`, `copyToAll`,
  `killAllOtherCopies`, `windowPrompt`/`xmonadPrompt`, `manPrompt`,
  `layoutScreens`/Grid, `rescreen`, `RotateVideoFloat`/`ToggleSizeVideoFloat`
  (FloatingVideos.hs), screen-corner toggling, mouse gestures.
- **`manageHook` className rules:** Signal/Element → comm workspace,
  vscodium → ide, browsers → web, vlc → side float, KeePassXC/easyeffects →
  admin. Port to per-WM "window rules".
- **X11-only hooks:** xcompmgr (compositor/fades), `xkill`, `xprop`, `xautolock`,
  `xmobar` status bar. All need replacements.
- **Layouts:** Tall, Mirror Tall, Accordion, Full (noBorders Floating),
  Tabbed — plus per-workspace layouts. Layout *names* map fine; "Accordion" and
  screen-corner toggling are xmonad-unique.

## Comparison table

Stars are approximate GitHub values (2026-08). "Maintained" is a read on
recent commit/release cadence and who stands behind it.

| WM | Display | Config style | Stars (≈) | Maintained | Portability of your keybindings |
|---|---|---|---|---|---|
| **Sway** | Wayland | i3 text config | 17.3k | ✅ Excellent — 84 releases, 1.12 (2026-05), last push 2026-07, 390 contributors, Linux Foundation society of maintainers | High — `$mod4` combos port ~1:1; leader chord via `mode {}`; window rules replace manageHook |
| **Hyprland** | Wayland | `hyprland.conf` | 38k | ✅ Excellent — frequent releases, large contributor base, biggest in class | High — flexible `bind`/`submap`; window rules replace manageHook; animations baked in |
| **Niri** | Wayland | TOML + keyfile | 26k | ✅ Active — v25.08 (Aug 2025); watch for post-Yalter creator transition | Moderate — i3-style key names; recommended keys don't map to custom layouts |
| **River** | Wayland | `riverctl` commands | 4.2k | ✅ Steady — small team, GitHub+Codeberg, created 2020 | Low — commands-based; use oxbow for tiling policy (rewrite) |
| **xmonad-wayland** | Wayland (River) | Haskell `.hs` | <10 (very small) | ⚠️ Very young/small — created late 2025; niche | Keys identical (StackSet + River), but no contrib/manageHook/X11 hooks — your config is mostly those |
| **waymonad** | Wayland | Haskell | 865 | ❌ Stalled — last significant activity ~2019–2020, wlroots pinned old | Niche; Haskell, not a straight upgrade |
| **Qile** | Wayland/X11 | Rust "as a program" | ~100s (solo) | ⚠️ Solo/small project — rewrite, not port | Philosophy-closest to xmonad, but you write it |
| **Råu** | Wayland | Elisp | very small | ⚠️ Solo/niche — reuses Emacs bindings | Best if your real interface is Emacs (org/journal) |

## GitHub maintenance notes

- **Hyprland (~38k):** the runaway leader. Biggest user base, frequent releases,
  heavy investment. Downside: GPU-heavy, and some find the visuals "too
  gimmicky." Most-visible of the group — you won't be stuck on an unmaintained
  compositor.
- **Sway (~17.3k):** the most *stable* and mature. Backed by a formal Society of
  Maintainers under the Linux Foundation, so maintenance isn't dependent on any
  one person. Slower release cadence than Hyprland by design — stability over
  features. This is the safe long-term pick.
- **Niri (~26k):** fastest-growing; very active development (v25.x series). One
  flag: the creator (Yalter) transitioned roles and there was community churn
  around who drives the project — great momentum, but worth knowing the
  maintenance story is more personality-driven than Sway's.
- **River (~4.2k):** small but healthy, dual-hosted on GitHub and Codeberg.
  Deliberately minimal (compositor ≠ window manager), so you pair it with a
  policy like oxbow — more work, more control.
- **xmonad-wayland (<10 stars, late-2025 repo):** the best keybinding match but
  the smallest/most-experimental. Read "no contrib/manageHook/X11 hooks" as a
  real maintenance risk for *your* config specifically.
- **waymonad (~865):** dormant; pinned to an old wlroots. Not recommended now.
- **Qile / Råu:** solo hobby projects. Fun and low-commitment, but no
  maintenance safety net.

## Candidates, ranked by keybinding portability

### 1. xmonad-wayland — closest keys, but your config is its weak spot
- Repo: github.com/cristiancmoises/xmonad-wayland — "XMonad window management
  policy for the River Wayland compositor, using XMonad's unmodified StackSet."
- **Key point:** keep your config idiom — `~/.xmonad/xmonad.hs` with
  `import XMonad.Wayland.XConfig`, `main = xmonad $ def { … }`, run
  `xmonad-wayland --recompile`. Same keybindings, same mod4, same StackSet.
- **Where your config breaks:** "no xmonad-contrib, no X11 hooks, no
  manageHook in this release." Your config is ~60% those three things, so
  key names survive but actions need reimplementing. Best if you trim the
  config — and accept a very small, young project.

### 2. Sway — best "rewrite that stays in the spirit"
- i3 drop-in replacement for Wayland. Config is plain text, `$mod + key` syntax.
- Your `$mod4` combos port almost 1:1 to config lines; descriptive keymaps
  become comments. Window rules replace `manageHook` (`windowrule v2
  class:…`).
- Leader chords: Sway supports `bindsym` chains and `mode {}` for a leader key,
  so `M-o S-j` is feasible.
- Non-ASCII keys: Sway matches on key *names*; ö/ä/ü are doable but verify the
  XKB key names for your layout — the spot where most ports stall.
- Status bar: Waybar replaces xmobar. Compositor fades: not built in (Sway is
  deliberately minimal).
- **Maintenance:** the safest long-term of the bunch (Society of Maintainers).

### 3. Hyprland — most flexible, most surface area
- C++ wlroots compositor; `bind = mod, key, command` in `hyprland.conf`.
- Rich keybind manager (modifiers, submaps, long-press, release,
  non-consuming). Leader/submaps map naturally to `submap`/modes.
- Window rules via `[windowrule]` replace `manageHook`. Very granular.
- Non-ASCII keys and custom modifiers supported; animations/blur baked in.
  Heavy GPU use; biggest community (won't be abandoned).

### 4. Niri — i3-style, but TOML + separate keyfile
- Column-based Wayland compositor (i3-config flavour). Keybindings in an
  INI-style keyfile; layout/config in TOML.
- Users report the *recommended* keybindings don't map well to custom layouts —
  you'll design your own. Good if you like i3-style key names and don't mind
  restructuring your leader chords.
- **Maintenance:** fast-moving (v25.x), but creator-transition risk.

### 5. Qile / River / oxbow — for the "configure-as-code" crowd
- **Qile:** ascetic, workspace-based, "configured as a program" (Rust) — the
  closest philosophy to xmonad, but you *write* it, not port it. Supports X11
  and Wayland.
- **River:** minimal wlroots compositor; config via `riverctl` commands.
  **oxbow** (OCaml) is a tiling policy for River (layouts incl. deck/spiral,
  per-tag config) — elegant but rewrite.
- Good keybinding freedom, zero keybinding continuity.

### 6. Råu — the "you already use Emacs" option
- Wayland WM written entirely in Elisp; external Wayland windows map to Emacs
  buffers. You can reuse your existing Emacs keybindings/mechanisms.
- Relevant because your whole org/journal workflow lives in Emacs
  (`openJournal` spawns emacs on `~/Nextcloud/*.org`). If Emacs is your
  interface, Råu could reuse most of your muscle memory — but it's a
  paradigm shift, not a WM migration.

## Gotchas that affect ALL of these

- **Non-ASCII key names (ö/ä/ü):** Wayland compositors match key names per
  XKB, not raw keycodes the X-server gave you. Verify each special key works
  before committing — this is the #1 reason these ports stall.
- **Leader chord `M-o S-j`:** needs leader/mode support. Sway (`mode {}`) and
  Hyprland (`submap`) handle it natively; Niri/River require more wiring.
- **`manageHook` className rules → window rules:** mapping is straightforward
  (class → workspace/rule) but window *class* values can differ between X11 and
  XWayland apps, so re-check each rule's className.
- **X11-only pieces you must replace:** xcompmgr (fades/compositor) → drop or
  use compositor-native; xkill/xprop → equivalents exist; xautolock →
  swaylock / wayland-lock; xmobar → Waybar (Sway) or eww/Hyprland-native
  (Hyprland).
- **Mod4 = left Super** is preserved everywhere, so the "mod key feel" is fine.

## Recommendation for jan

- **Smoothest realistic migration with least new mental model + safest
  maintenance:** **Sway + Waybar.** Your `$mod4` combos port 1:1, window rules
  replace `manageHook`, leader chords via a `mode` block, re-verify ö/ä/ü.
- **Want to keep Haskell authoring + identical keys:** **xmonad-wayland**,
  accepting you'll trim contrib/manageHook/X11-hook usage and that it's a very
  small project.
- **Want maximum polish and are fine rewriting:** **Hyprland** (biggest, most
  actively maintained community).
- **Emacs is your real interface:** look at **Råu**.
