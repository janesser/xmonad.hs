# HA — Prepare Activation: Tado + Tuya

**Status:** prepared — HA is live on `raspberrypi`; both integrations are
**UI config-flow only** (no YAML, no restart needed before setup). Ready to
activate whenever the credentials are at hand.

**Next:** you assist with the credential steps (see §4). Everything below is
preparation, no action required from you yet.

---

## 0. Current state

- **Host:** `raspberrypi` (Pi 400, 4 GB, Debian 13)
- **HA:** `homeassistant` service, rootless podman container, `stable` image
  (HA 2026.10.4), `http://raspberrypi:8123` (also `localhost`/`127.0.0.1`)
- **Auth:** HA auth is ON (API returns `401` without a token). The UI login
  works normally.
- **Privilege:** container still runs `--privileged` (per the install notes) —
  no impact on cloud integrations.
- **Existing integrations:** sun, analytics, go2rtc, backup (from the ad-hoc
  container migration).

## 1. Activation method

Both integrations use the **built-in UI config flow** — no YAML editing, no
restart, no HACS:

1. **Settings → Devices & services → Add Integration** (bottom right)
2. Pick **Tado** or **Tuya** from the list
3. Follow the on-screen flow

Auto-discovery works for both (they are cloud integrations, so discovery is
account-based, not device-based).

## 2. What each integration does

| | **Tado** | **Tuya** |
|---|---|---|
| **Purpose** | my.tado.com cloud — climate zones, presence, weather, energy | Tuya Smart / Smart Life cloud — all devices in the app |
| **Entities** | `climate.*`, `sensor.*_humidity`, `binary_sensor.*`, `weather.*`, `device_tracker.*` | `switch.*`, `light.*`, `sensor.*`, `scene.*`, … (all platforms except lock/remote) |
| **IoT class** | Cloud Polling | Cloud Push |
| **Key caveat** | Rate-limited API (daily quota, resets 12:00 CET); humidity values can be stale; **new Tado X devices need Matter** | SDK doesn't expose all Smart Life functionality; re-auth may be needed after iot.tuya.com ToU updates |

## 3. What's needed before you start

Nothing on the HA side. Both are ready.

## 4. What you need to have ready (credential steps)

### Tado
- Your **my.tado.com** account credentials (email + password)
- During setup, HA generates a **Device Code** and shows a URL to
  `my.tado.com` — you open that URL (on any device), confirm the code, and
  log in. No extra app needed.
- **Note:** as of 2026-01-01 Tado rate-limits the API per daily quota. If
  auth fails or data doesn't update, check the quota (resets 12:00 CET).

### Tuya (Smart Life)
- Your **Tuya Smart** or **Smart Life** app account (the one your devices are
  registered under)
- **User Code** — from the app: **Me → ⚙️ → Account and Security → User
  Code** (shown at the bottom)
- A **second screen** (phone/tablet/PC) to scan the QR code shown in HA
- During setup: scan the QR code with the app, then enter the User Code

### Both
- A stable network connection from `raspberrypi` to the cloud
- The dryer (Smart Life) must be visible in the Smart Life app **before**
  adding the Tuya integration in HA

## 5. Activation procedure (to run when ready)

```
1. Open https://raspberrypi:8123 in a browser
2. Settings → Devices & services → Add Integration
3. Add Tuya → follow flow (QR scan + User Code)
4. Add Tado → follow flow (my.tado.com OAuth)
5. Verify entities appear (States page / Developer Tools)
```

Order doesn't matter. A restart is only requested by HA if a config change
requires it (rare for cloud integrations).

## 6. Link to the bath-humidity plan

This activation unblocks **ha-bath-humidity.md**: Tado humidity sensor +
Smart Life air dryer switch, wired through HA automations (the `humidity
crossed threshold` trigger, with debounce + auto-off).

## 7. Post-activation checks

- Tado: confirm `climate.*` and `sensor.*_humidity` entities exist and update
  (watch for 20+ min; the humidity value is known to be stale — verify against
  reality)
- Tuya: confirm the air dryer shows as a `switch.*` entity; reload the
  integration after adding new devices to the Tuya app
- Presence detection (Tado): the device tracker needs the Tado app's
  `Geolocation` permission on the tracked phone

## 7b. Backup (regular, to Nextcloud)

A daily backup is now deployed: **user systemd timer `ha-backup.timer`** runs
`~/.local/bin/ha-backup.sh` as `jan` at **03:00** (Persistent=true, so it
catches up after reboots). Userspace — the HA config is user-owned, so the
backup runs in userspace too (no root/sudo involved).

- **Source:** `~/.local/share/homeassistant/config/` (the container's
  persistent `/config` volume — integrations, DB, automations, tokens)
- **Destination:** `~/Nextcloud/ha-backup/homeassistant-config_<YYYY-MM-DD_HH-MM-SS>.tar.gz`
  (synced by your Nextcloud client)
- **Retention:** last 7 daily backups
- **Restore:** copy the archive back to
  `~/.local/share/homeassistant/config/` (extract `tar xzf`), then
  `systemctl --system restart homeassistant`

One caveat: the DB is copied while HA runs — SQLite handles concurrent copies
fine, but the snapshot reflects the state at tar time.

## 8. Your Tuya sensor — the Tado limitation is already solved

You mentioned a Tuya sensor that overcomes the Tado humidity limitation.
This is the robust engineering choice from `ha-bath-humidity.md` (mitigation
#1): a real Tuya/Smart Life humidity sensor in the bath, updating every
1–2 min, local or via the Tuya cloud. It is a better trigger than the
stale Tado reading — keep it as the primary humidity source for the
bath-humidity automation.

## 9. Your Tuya Zigbee 2.0 bridge — direct Zigbee in HA

**Short answer:** the Zigbee devices themselves can talk to HA directly;
the Tuya bridge is not needed. But the bridge hardware is generally a
**dead end** for direct use.

### How it works
Tuya Zigbee devices (sensors, switches, plugs) are standard Zigbee 3.0
devices. They can be paired directly to a Zigbee coordinator in HA —
**ZHA** (Zigbee Home Automation, built into HA) or **Zigbee2MQTT** (needs
a separate add-on) — instead of going through the Tuya bridge and Tuya's
cloud. This gives:
- local control (no cloud dependency),
- no Tuya app needed for these devices,
- all device functions that the Zigbee standard exposes.

### What you need
- A **Zigbee coordinator/dongle** in the HA box (raspberrypi). USB
  coordinators are cheapest (e.g. Sonoff Zigbee 3.0 Dongle Plus, or any
  CC2652/EP-CC2652 stick). The Pi 400 has USB ports. If the Pi is in a
  metal case, get a USB extension cable so the dongle isn't RF-shielded.
- **ZHA** is the simplest path: *Settings → Devices & services → Add
  Integration → Zigbee Home Automation → select your coordinator → add
  devices*. Pairing is the same as any Zigbee device (usually hold the
  button ~5s until it blinks).
- **Zigbee2MQTT** is an alternative if you want more device-specific
  handling (better support for some Tuya quirks), but it needs an MQTT
  broker and runs as a separate add-on.

### What about the Tuya bridge itself?
- **Generally: no.** The Tuya Zigbee 2.0 bridge is a closed system — its
  Zigbee coordinator talks only to Tuya's cloud. It cannot be used as a
  ZHA/z2m coordinator without hardware hacks (flashing the NCP with
  EmberZNet is possible on a few models, e.g. the Tuya ZS3L, but that's
  an advanced project).
- **Exception:** a handful of Tuya-branded gateways (e.g. the Tuya
  Ethernet Zigbee Gateway TYGWZ-01, some Lidl/Silvercrest units) use a
  standard CC2531/CC2652 chip and **do** work with ZHA out of the box.
  Check the bridge's exact model number against
  [zigbee.blakadder.com](https://zigbee.blakadder.com/) to be sure.
- **Do not** pair the same Zigbee device to both the Tuya bridge and the HA
  coordinator — Zigbee allows only one coordinator per device. Reset the
  device (factory reset via the button hold) before pairing it to HA.

### Decision point
If the bridge is just a Tuya-cloud hub, you can **leave it as-is** (it
controls the devices in the Tuya app) **and** pair the same devices to HA
via a coordinator — but only one coordinator per device, so you must choose:
- **HA coordinator** (local, cloud-free, best for automations) — or
- **Tuya bridge** (Tuya app control, cloud-dependent)

Since HA is your automation glue, the coordinator path is the better fit.

### Next step
Check the exact model of your Tuya bridge (sticker on the bottom) and let
me know — I can verify whether it's one of the rare ZHA-compatible ones,
and whether your Zigbee devices are in the ZHA/z2m supported-device list.
