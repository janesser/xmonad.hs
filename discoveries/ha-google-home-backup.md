# Google Home Settings Backup & Restore

**Status:** proposal — scriptable **export** of Google Home device settings to a
local snapshot, and **restore** of those settings from that snapshot.
**Target scripts:** two shell scripts in `dot_local/bin/` (i.e. `~/.local/bin/`),
managed by chezmoi, mirroring the other `executable_*` helpers there.

---

## TL;DR

Google Home devices expose their tunable *settings* only through a small set of
undocumented local APIs and third-party tooling — there is **no official CLI or
published API**. The realistic path is a **local device API** read via a community
library, snapshot the values we care about to a local file, and be able to re-apply
them. The big open question is **which settings** are actually reachable, because
that decides whether the whole thing is feasible today or needs a specific device.

Two things to decide before building:
1. **Which settings** do we actually want to back up? (Section 1)
2. **Which tool/transport** reads and writes them? (Section 2)

Once those are pinned, the two scripts are straightforward:

- `google-home-backup` → read current settings → write a local snapshot (JSON).
- `google-home-restore` → read a local snapshot → re-apply the settings.

---

## 1. What "settings" even means for Google Home

This is the crux and the most likely source of rework. "Google Home settings"
could mean several very different things, and reachability differs a lot:

| Setting | Reachable locally? | Notes |
| --- | --- | --- |
| **Ambient / Always-On conditions** (motion sensor, weather, clock style, "Good Morning/Good Night" greetings, sleep schedule) on **Nest Hub** | **Yes, via local device API** — this is the main target. These are the "Ambient Settings" you edit in the Google Home app under a Nest Hub's settings. | Requires a Nest Hub (or similar always-on display). Speakers alone don't have these. |
| **Volume / mute** | Partially — `python-googlehome` can set volume on some devices. Not a rich "settings" set. | Best for media/device volume, not config. |
| **Alarms / timers** | Alarms **no** (cloud-only via app). Timers are transient. | Not a good backup target. |
| **Wake word, mic mute, device rename, room assignment** | Cloud-app only. No reliable local API. | Skip / out of scope. |
| **Network / Wi-Fi, factory reset** | Never programmatic. | Out of scope. |

**Working assumption (to be confirmed):** the meaningful backup set is the
**Ambient / Always-On settings on a Nest Hub** (the things that break your routine
and aren't obvious to re-derive). If your real intent is something else (volume,
alarms, whole-home device config), the tooling below may need to change — see
Section 2 caveats.

---

## 2. Mechanism / transport options

There is **no official Google Home CLI.** Options, in rough order of practicality
for a personal dotfiles setup:

### Option A — Local Nest API via community tooling (recommended to explore)
Nest Hubs expose an **internal local API** (HTTPS on the device, historically
`https://<device-ip>:8443/...`, mTLS-enticated). Community projects read/write
Ambient Settings through it:

- **[`jombeck/googlehome`](https://github.com/jombeck/googlehome)** (`python-googlehome`)
  — pip library, mDNS/SSDP discovery + local API. Good for **volume/media**; its
  coverage of Ambient *Settings* is thin.
- **`google-home-settings` / `ghome`-style tools** — community scripts that drive
  the local Ambient Settings API directly. Best fit for our target (Section 1),
  but each is unofficial and tied to a specific device firmware era.

**Pros:** local, no cloud creds, works offline, fast.
**Cons:** undocumented and firmware-sensitive; values/URLs shift between Nest
firmware versions; may need packet-sniffing to confirm the current endpoints.

### Option B — Google Smart Home / Home Graph API (cloud)
Full Google account OAuth or a Home Graph service account. Powerful but heavy:
requires a Google project, OAuth consent, and (for Home Graph) device registration.
**Overkill** for local settings backup and a poor fit for a shell helper. Only
worth it if you later need cloud-wide config.

### Option C — Google Home app automation only (no script)
Some settings can be snapshotted manually in the app, but there's no export —
so this can't back them up. Out of scope for a script.

**Recommendation:** pursue **Option A**, but first confirm (Section 3) that the
Ambient Settings API is reachable from this network against your specific device.
If it isn't, fall back to what *is* reachable (e.g. volume via `python-googlehome`).

---

## 3. Feasibility probe (do before building)

Before committing to the scripts, run one quick check to see what's reachable:

1. Confirm the target device is a **Nest Hub** (or has Ambient Settings). If it's
   a speaker, "Ambient settings" don't exist — reconsider what to back up.
2. From this machine, confirm you can reach the device's local API:
   - Find the device IP / hostname on the LAN.
   - See if the known local endpoint answers (community projects usually document
     the URL; e.g. an `/ambient_settings` or similar path on `:8443`).
   - If discovery/mTLS blocks you, note the blocker explicitly.
3. Pick the concrete list of settings to snapshot (e.g. motion detection, weather
   location, clock style, Good Morning/Good Night, sleep schedule).

If the probe fails, the scripts can still be written against a chosen tool, but
their runtime correctness is **unknown** until probed — flag this clearly rather
than shipping scripts that silently do nothing.

---

## 4. Proposed script design

### Storage / snapshot format
- Default snapshot dir: `~/.config/google-home-snapshot/`
- Default snapshot file: `~/.config/google-home-snapshot/<device>.json`
- JSON: one object per setting with its current value + a timestamp, e.g.:
  ```json
  {
    "device": "Nest Hub (Kitchen)",
    "snapshot_at": "2026-09-21T18:30:00+02:00",
    "settings": {
      "motion_detection": true,
      "weather_location": "Berlin",
      "clock_style": "digital",
      "good_morning_enabled": true,
      "good_night_enabled": true,
      "sleep_schedule_enabled": false
    }
  }
  ```
- Keep it **plain JSON** (no encryption needed; it's local config). If you later
  want it encrypted, chezmoi can `cz add --encrypt` the file.

### `executable_google-home-backup.sh`
1. Resolve target device (arg or prompt).
2. Read current settings via the chosen local tool/library.
3. Write the JSON snapshot to the default (or `--out <path>`) location.
4. Print a short summary of what was captured.

### `executable_google-home-restore.sh`
1. Accept a snapshot file path (arg, default the latest in the snapshot dir).
2. Parse JSON, iterate settings.
3. Re-apply each setting via the same local tool/library.
4. Print what was applied; error out (non-zero) if any setting fails, so the
   restore is observable.

### Cross-cutting
- **Bash**, like the other `~/.local/bin` scripts. `set -euo pipefail`.
- **Dependency:** the local-tool/library (e.g. `python3 -m googlehome` or a
  community CLI). If it's a Python lib, that's a runtime dep — consider pinning
  it and noting it in the script header / a comment. (Not a chezmoi package unless
  you want `cz` to manage it.)
- **Idempotency / safety:** restore should be explicit (takes a file), never
  auto-restore on boot. Optionally keep the last-good snapshot before overwriting.

---

## 5. Open decisions (need your input)

- **Which settings?** Ambient/Ambient-Conditions on a Nest Hub (my default
  assumption), or something else (volume, alarms, whole device config)?
- **Which device(s)?** One Nest Hub or several? (Determines single-file vs.
  per-device snapshot tree.)
- **Transport confirmed?** Have you successfully talked to a device's local API
  before, or is this the first probe? (Affects how confident we can be.)
- **Language:** bash wrapper calling a local tool (recommended) vs. a pure-Python
  implementation?
- **Snapshot location:** `~/.config/google-home-snapshot/` default is fine, or a
  specific path/format you already prefer?
- **Dependency handling:** keep the local tool as an external runtime dep, or
  manage it via chezmoi packages?

---

## ✅ Status: —

Not implemented yet. Blocked on Section 3 (feasibility probe) and the open
decisions in Section 5. Once the settings list and transport are confirmed, the
two `executable_google-home-*` scripts are a short, mechanical addition to
`dot_local/bin/`.
