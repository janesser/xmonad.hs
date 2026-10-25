# HA Bath Humidity Automation

**Status:** proposal — link **Tado** humidity → **Smart Life** air dryer via Home Assistant.
**Trigger:** measured bath humidity > 75%.
**Goal:** automatic counter-measure (start the air dryer) with sensible auto-off.

---

## TL;DR

The two ecosystems (Tado cloud + Smart Life/Tuya) don't talk to each other directly.
Home Assistant (HA) is the glue: it reads the Tado humidity sensor and flips the Smart
Life switch. The cleanest implementation is the native **`humidity.crossed_threshold`**
trigger (HA 2024.x+), set to cross **above 75%**, driving the switch **on**; a
`delay` + turn **off** handles auto-shutoff.

## The missing piece first

There is **no Home Assistant installed on this machine** (checked). This automation
runs wherever your HA instance lives (a always-on box / NAS / Raspberry Pi).
- If you already run HA → wire it into your existing config, skip section 0.
- If you don't → decide on a host first (section 0). Everything below assumes HA exists.

### Option A (recommended): Home Assistant
- **Pros:** first-class Tado + Smart Life (Tuya) integrations, visual editor, automations, templates.
- **Cons:** need an always-on device.

### Option B: Tuya-only, skip Tado
- Put a cheap **Tuya/Zigbee humidity sensor inside the bathroom** and drive the Smart
  Life dryer directly from the Tuya app / Tuya Local. No HA needed. Tado becomes
  irrelevant. **Simplest no-HA path** but you lose the "use the Tado reading you already have."
- Good fallback if HA turns out to be overkill.

### Option C: IFTTT / n8n / Node-RED
- Possible but messier: Tado → webhook → IF. Tado has no native IFTTT trigger for
  humidity, so you'd need polling. Only consider if you already run one of these.

---

## 0. Host the automation (if going with HA)

Any always-on device. Typical choices in order of fit:
- **HassOS / HA Supervised** on a mini-PC or Pi (best).
- **Docker** `homeassistant/home-assistant` container.
- Existing NAS VM (DiskStation/QNAP can run Docker containers).

Verify once it's up: `http://<ha-host>:8123`.

## 1. Connect the two devices

**Tado (read):**
1. Settings → Devices & Services → Add Integration → **Tado**.
2. Sign in with your Tado account. This exposes, per zone, a `climate`, and — crucially —
   a **`sensor.tado_<zone>_humidity`** entity (official integration creates it; see caveat).

**Smart Life / air dryer (act):**
1. Smart Life devices are integrated via **Tuya** (built into HA since 2024.2; the old
   standalone "Smart Life" integration was folded into core).
2. Settings → Devices & Services → Add Integration → **Tuya** → sign in with the same
   account you use in the Smart Life app. Your air-dryer switch becomes a `switch.*` entity.
3. If the dryer is slow/ unreliable over cloud, add the custom integration
   **[tuya-local](https://github.com/make-all/tuya-local)** for local control (HACS).
   (Doesn't stop cloud sync; it improves latency/reliability.)

Confirm the two entity ids you'll reference:
- Humidity: `sensor.tado_bath_humidity` (name it clearly)
- Dryer:    `switch.bath_air_dryer`

## 2. The automation (UI or YAML)

### 2a. Native threshold trigger (cleanest)

The `humidity.crossed_threshold` trigger fires when a humidity reading crosses a zone
boundary you define — exactly "above 75%".

```yaml
# automations/bath-humidity-dryer.yaml
alias: Bath humidity -> start air dryer
description: Start the air dryer when bath humidity crosses above 75%.
trigger:
  - platform: humidity.crossed_threshold
    entity_id: sensor.tado_bath_humidity
    above: 75
    for:
      minutes: 5          # debounce: ignore short spikes (e.g. a hot shower burst)
condition: []
action:
  - service: switch.turn_on
    target:
      entity_id: switch.bath_air_dryer
mode: single
```

Add **auto-off** (dryers usually shouldn't run forever). Two clean ways:

- **Timer mode** — after turning on, wait then switch off:
  ```yaml
  mode: restart          # lets a new spike reset the run
  action:
    - service: switch.turn_on
      target:
        entity_id: switch.bath_air_dryer
    - service: timer.start    # or just delay below
      target:
        entity_id: <dryer_timer>
  ```
- **Simple delay** (good enough for most dryers):
  ```yaml
  mode: restart
  action:
    - service: switch.turn_on
      target:
        entity_id: switch.bath_air_dryer
    - wait_for_trigger        # or `delay`
      ...
  ```
  Practical delay version:
  ```yaml
  trigger:
    - platform: humidity.crossed_threshold
      entity_id: sensor.tado_bath_humidity
      above: 75
      for: {minutes: 5}
  mode: restart
  action:
    - service: switch.turn_on
      target: {entity_id: switch.bath_air_dryer}
    - service: homeassistant.turn_off
      target: {entity_id: switch.bath_air_dryer}
      delay: {minutes: 30}   # run for up to 30 min, then auto-off
  ```

### 2b. Equivalent with a template trigger (works on older HA)

If `humidity.crossed_threshold` isn't available:

```yaml
trigger:
  - platform: numeric_state
    entity_id: sensor.tado_bath_humidity
    above: 75
    for: {minutes: 5}
```

(Downside: `numeric_state` only fires on crossing *up*, so auto-off must be time-based
as above. The humidity trigger also handles the fall-back.)

### 2c. Ready-made blueprint

The community "Bathroom Humidity Exhaust Fan" blueprint (v2.7) does exactly this with a
derivative helper (turns on when humidity is *rising fast*, caps at a max humidity, and
auto-shuts-off after a delay). Worth importing if you want back-to-back-shower handling:
https://community.home-assistant.io/t/bathroom-humidity-exhaust-fan/509992

---

## ⚠️ Critical caveat: the Tado humidity sensor is often stale

This is the thing most people miss. Known issues:
- Tado **polls the cloud every ~20 min**, and the **humidity value "almost never
  updates"** — it can sit frozen for hours/days even when real humidity changes
  (HA core issue #63806, #30585).
- If you rely on this sensor, your "above 75%" automation may **never fire**, or fire far
  too late.

**Mitigations (pick per how much you trust the Tado reading):**
1. **Prefer a real bathroom sensor.** Put any cheap Tuya/Zigbee humidity sensor *inside*
   the bath. It updates every 1–2 min, locally, and drives the dryer directly. Treat the
   Tado reading as a secondary/backup only. (This is the robust engineering choice.)
2. **Add a fallback timer** so the automation can't hang forever: a `time_pattern` or
   daily check that turns the dryer off if it's been on too long.
3. **Don't gate solely on the threshold** — also allow a manual override switch so you can
   run the dryer regardless of the possibly-frozen sensor.
4. If you *do* trust Tado, verify the entity actually moves: check its history in HA for
   a flat line. If flat, switch to mitigation #1.

## 3. Debounce & safety (don't skip)

- **`for:` delay (5 min):** ignores brief spikes from a hot shower before the room
  actually saturates. Tune to your bathroom's venting.
- **Auto-off:** dryers vary — check the spec for max continuous run time to avoid
  overheating/drying the element out. Cap it.
- **Hysteresis:** threshold trigger already gives a single cross; if you want hysteresis
  (e.g. off again below 65%), add a condition or a second automation on the *below* branch.
- **Manual override:** expose the switch so a person can run it without tripping the sensor.

## 4. Testing plan

1. Add Tado zone + Tuya switch; confirm both entities exist and have sane names.
2. In Developer Tools → States, watch `sensor.tado_bath_humidity` for **20+ min** —
   confirm it changes. If frozen, deploy a real bath sensor (mitigation #1).
3. Temporarily lower `above:` to 50 and raise the `for:` window, or simulate, to confirm
   the dryer actually turns on.
4. Verify auto-off fires after your delay.
5. Set back to `above: 75`, `for: 5 min`.

## Voice control (Google Home) — you already have this

You use "OK Google, what's the humidity in the bathroom?" and "OK Google, start the air
dryer" today. This is **not** in conflict with any option below — it's a separate layer
(manual, on-demand) from the automated >75% counter-measure.

Two useful facts:

- **Google Home now supports humidity-threshold routines.** A routine can be *started*
  when "indoor humidity crosses a threshold". So the auto counter-measure **can** live
  entirely inside Google Home — *if and only if* the triggering sensor and the dryer are
  devices Google Home natively understands (i.e. **Tuya/Smart Life**). Tado is **not**
  natively in Google Home, so a routine triggered on the **Tado** reading won't work here.
- **Keep the voice commands either way.** If you go HA, the built-in **Google Assistant**
  integration (or Nabu Casa) lets you *expose* HA entities back to Google Assistant, so
  "OK Google, start air dryer" and even "what's the bathroom humidity" keep working, now
  served by HA instead of the Smart Life app. (Settings → Voice assistants → Expose.)

### How this changes the recommendation

With Google Home supporting humidity triggers, **Option B is now the strongest no-HA path**
for you specifically:

1. Add a cheap **Tuya/Smart Life humidity sensor inside the bathroom** (fast, local, in
   Google Home's world).
2. In the Google Home app create a routine: *when indoor humidity rises above 75%*
   → *turn on the air dryer*. Auto-off via a second routine / the dryer's own timer.
3. Your existing "start air dryer" voice command is unchanged; the "what's the humidity"
   query now reports the in-bath Tuya sensor instead of the (stale) Tado one.

Only bother with **HA** if you later want cross-ecosystem logic Google Home can't do
(e.g. "dryer on only if a Tado reading disagrees", multi-device scenes, templates). For
this single-room counter-measure, Google Home + one Tuya sensor is simpler and keeps your
voice flow intact.

## ✅ Status: Google Home automation set up (2026-09)

The Google Home humidity routine is **now in place** — this is the live implementation
(Option B). Configuration:

- **Trigger:** Google Home routine, *when indoor humidity rises above 75%*, → *turn on
  the air dryer*.
- **Trigger source:** a Tuya/Smart Life humidity sensor the bathroom (in Google Home's
  native world — Tado can't trigger this).
- **Voice commands unchanged:** "OK Google, start the air dryer" still works; the "what's
  the humidity" query reads the in-bath sensor.

**Still to validate (see the polling caveat above):**

- Google does **not publish** how often the routine re-checks. Worst case there's a delay
  before it fires. Confirm the real lag empirically (hold something humid near the sensor
  and watch fire time; check the routine's run history in the Google Home app).
- Confirm whether the sensor **pushes** updates (near-real-time) or only answers on
  Google's cloud poll (slower, unknown interval).
- If the dryer needs a hard auto-off, do it on the dryer's own timer or a separate
  time-based routine — don't depend on Google re-polling.
- Note the re-trigger suppression: once fired, Google won't re-check for a while, so
  humidity hovering right at 75% won't cause rapid on/off cycling.

## 5. Open decisions

- **HA host** — do you already have one? (None detected here.)
- **Trust Tado vs. add a real bath sensor** — I'd add a real sensor regardless; the Tado
  reading is a weak trigger.
- **Dryer max run time** — check the device spec before picking the auto-off delay.
- **Single threshold (75%) vs. hysteresis** (on@75 / off@65).
