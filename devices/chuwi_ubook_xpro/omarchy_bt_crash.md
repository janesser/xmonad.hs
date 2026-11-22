# WirePlumber SIGSEGV on resume from S3 (Omarchy 4.5, PipeWire 1.6.9)

2026-11-22 — flowerpower (`chuwi_ubook_xpro` notes: this bug affects the Omarchy fleet, not one machine).

## The crash

wireplumber PID 181952 crashed with `SIGSEGV / si_code SI_KERNEL` at 07:47:43 CET, ~2 s after S3 resume (lid opened 07:47:40, `PM: suspend exit` logged 07:47:42, Intel Bluetooth firmware reloaded).

Backtrace (crash site in `libspa-bluez5.so`, no Arch debuginfo):

```
#0  libspa-bluez5.so +0x4fdd8     ← faulting instruction: mov %rax,(%rdx), rdx holds ASCII "bluez5.p" — a stale pointer into freed memory
#1  libpipewire-0.3.so.0 +0x6c8ce
#2  pw_load_spa_handle()
#3  wp_spa_device_new_from_spa_factory(factory_name="api.bluez5.device")
#4  wireplumber-module-lua-scripting.so (Lua event dispatch)
...
#19 g_closure_invoke ()
#21 wp_transition_advance (libwireplumber-0.5.so.0)
```

The plugin dereferenced a struct field (`s->device`) that had been freed and reallocated — the freed object's memory now holds part of a device address string ("AC:BF:71" or similar) or a property key, so the read produced a non-canonical address (general protection fault, `si_code 128`).

## Evidence it is a known upstream bug, not a one-off

- Same three crash sites reported on exactly this version combination (PipeWire 1:1.6.9-1 / WirePlumber 0.5.18-1 / BlueZ 5.87-2): `+0x4fdd8`, `+0x4125e`, `+0x659f6` — each within ~1 s of `PM: suspend exit`, across multiple Omarchy machines ([Omarchy issue #11280](https://github.com/omacom/omarchy/issues/11280), Ubuntu LP 2170599).
- Same call path `pw_load_spa_handle → wp_spa_device_new_from_spa_factory` reported by several machines (device creation); others via `wp_spa_device_enum_params_sync` (profile enumeration) — more than one call path is affected.
- Register-level reports in that thread decode the identical class of fault: stale `s->device` dereferenced as if valid, freed heap memory containing device-name/address strings.
- This machine itself: 7 wireplumber coredumps in the previous two weeks (Nov 4 – Nov 22), all after resume.

## Cause

WirePlumber 0.5.17 removed SPA device/node objects asynchronously; PipeWire's `libspa-bluez5` in 1.6.x still assumes synchronous removal, so on resume (or rfkill/adapter teardown) it follows a pointer to a device object that has already been freed — use-after-free in the BlueZ audio plugin.

Upstream fix: commit [b7246d0d](https://github.com/PipeWire/pipewire/commit/b7246d0d5ddc79cbab06e7e55f6dda37c446118d) "bluez5: deal with asynchronous device and node removal" (MR !3004, fixes PipeWire issue #5471), released in **PipeWire 1.6.11**. Not in 1.6.9 — this machine is still affected.

## Impact

- Transient only: wireplumber auto-restarts (`Restart=on-failure` at 07:47:44) and the audio graph is rebuilt; Bluetooth endpoints re-register ~6 s later.
- Every crash tears down playback: apps lose their sink and stop. No user data lost.

## Fix / mitigation

Upgrade `pipewire`/`pipewire-audio` to **≥ 1:1.6.11** — the bug is in PipeWire's third-party BlueZ plugin, not in Omarchy's config or scripts (the shipped `bluez5.auto-connect` drop-in merely makes resume-time device enumeration more likely).
If the stable mirror has no update, follow up on [Omarchy issue #11280](https://github.com/omacom/omarchy/issues/11280) — that thread contains independent reproductions from 6+ machines and a draft comment for maintainers.

To silence the "Process crashed:" notification for this one program:
```bash
omarchy-crash-mute '/usr/bin/wireplumber'    # mute
omarchy-crash-mute '/usr/bin/wireplumber' off  # unmute
```
