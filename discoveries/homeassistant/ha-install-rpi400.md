# HA Install — Raspberry Pi 400 (chezmoi-managed container)

**Status:** active — staged in chezmoi (unit + 2 run scripts); next: `cz apply` on raspberrypi, then verify (UI on :8123, config migrated from the ad-hoc container, reboot persistence).

## TL;DR

Home Assistant runs on this box (raspberrypi — Pi 400, 4 GB, Debian 13) as a
**rootless podman container** under a **system systemd unit** (`User=jan`),
deployed and updated by chezmoi. Host-gated to `raspberrypi`; no other host is
touched. Replaces the ad-hoc container from 2026-10-18 (onboarding + default
integrations preserved via config migration) and supersedes the draft
`docker-compose.yml` in this directory (kept for reference only).

## Decisions (2026-10-19)

| Question | Choice | Why |
| --- | --- | --- |
| Engine | rootless podman 5.4.2 | the `docker` CLI here is a podman alias (no real docker daemon); rootless matches the existing ad-hoc container — same store, seamless migration |
| Service model | system unit, `User=jan` | survives boot without user lingering (same pattern as llama-cuda); no `loginctl enable-linger` needed (and it is not in the scoped sudo drop-in) |
| Image | `docker.io/homeassistant/home-assistant:stable` | already pulled (2.45 GB); unqualified-search-registries=docker.io was set by run_once_3_devtools_3podman.sh |
| Privilege | `--privileged` | official container-install baseline; the ad-hoc container ran privileged. Follow-up: drop once integrations are proven |
| Config | `~/.local/share/homeassistant/config` → `/config` | persistent, user-owned, survives container recreation |
| Network | `-p 8123:8123` (all interfaces) | LAN access for phone/desktop UI; HA auth is on. pasta networking; port publishing verified to work without XDG_RUNTIME_DIR (2026-10-19, throwaway container) |
| TZ | `Europe/Berlin` | matches host |

## Files (all new, host-gated to raspberrypi)

- `etc/systemd/system/homeassistant.service` — unit (source blob; `etc/` is
  chezmoi-ignored, so it is deployed by the run script, not `cz apply` directly)
- `.chezmoiscripts/run_onchange_hass_install.sh.tmpl` — install/enable +
  one-time ad-hoc container migration; change-gated on the unit's SHA256
  (no sudo on unchanged applies)
- `.chezmoiscripts/run_9_update_hass.sh.tmpl` — every `cz apply`: cheap
  `:stable` manifest check, restart only when the image ID changed

## Update flow

`cz update` is the update window: `run_9_update_hass.sh` pulls `:stable` (a
no-op when current) and restarts the service only when the image ID changed.
HA's in-UI update button does not work for container installs (no container
engine inside) — ignore its update notification.

Note: `run_9_cleanup_podman.sh` runs `podman system prune -f`, which removes
*stopped* containers — a stopped HA container may get pruned, but the unit
recreates it on the next start from the host config volume. No data risk
(config lives on the host, not in the container).

## Migration (runs on first apply)

The ad-hoc container (created 2026-10-18 23:51, privileged, no published port,
no volume; HA 2026.10.2; onboarding done; entries: sun, analytics, go2rtc,
backup) is stopped, its `/config` copied to the persistent volume, and removed
— only when it still has zero mounts and the target dir is empty; otherwise
the script refuses and asks for manual handling.

## Mid-term roadmap (separate increments — not built)

1. **SmartLife/Tuya** (HA cloud integration) → unblocks `ha-bath-humidity.md`
   (Tado humidity → Smart Life dryer automation).
2. **Tado** (cloud, OAuth) → humidity/temperature sensors + heating control.
3. **Voice** — HA Voice/Assist: local STT (faster-whisper) + Piper TTS on this
   box (4 GB Cortex-A72 is fine for STT/TTS); LLM conversation via the llama
   backend (cyberkleiber V100, OpenAI-compatible API over LAN).

## Follow-ups / hardening

- Drop `--privileged` once all integrations work without it (Tuya/Tado/voice
  are cloud/in-process — expected to work unprivileged).
- Optional: HA's built-in `backup` integration is already enabled — pick a
  backup destination (this box or nextcloud) and a schedule.
- Reboot verification pending (the gold standard for the boot-persistence claim).
