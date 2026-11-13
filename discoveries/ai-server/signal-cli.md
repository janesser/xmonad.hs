# Signal CLI Integration — State & Handover

## Summary

signal-cli 0.14.10 is installed and running on **raspberrypi** with the same account. The integration **is now in chezmoi**: two units (`signal-receive` + `signal-pi`) are deployed via a hostname-gated run script.

Status: the daemon (`signal-receive.service`, port 8081) and the persistent bot listener (`signal-pi.service`, `pi --print` with the signal env vars in its unit `Environment=`) are both running. The remaining verification is the end-to-end message flow — which needs the phone.

The listener's env vars are now in `signal-pi.service`'s `Environment=` lines; they were previously only set via a manual edit on cyberkleiber's `pi-memory-hub.service` (a dead config, no pi instance there).

## What was done (manual, outside chezmoi source)

- **raspberrypi**: installed `openjdk-25-jdk` (25.0.5), signal-cli 0.14.10 (`/opt/signal-cli-0.14.10`, linked to `/usr/local/bin/signal-cli`), systemd unit `signal-receive.service`
- **Account**: migrated from cyberkleiber (`~/.local/share/signal-cli/data/258148` + `accounts.json` — the key material) to raspberrypi; cyberkleiber's old signal-receive.service stopped. Running the same account on two hosts desyncs/duplicates messages.
- **Environment**: added `PI_SIGNAL_ACCOUNT`, `PI_SIGNAL_DAEMON_URL=http://raspberrypi.local:8081` (+ `PI_SIGNAL_PRIMARY=true` on raspberrypi) to `/home/jan/.profile` on both machines, and to `pi-memory-hub.service`'s Environment on cyberkleiber.
- **No chezmoi change**: all edits were direct renders — `~/.local/share/chezmoi/` is clean.

## Current state (verified from cyberkleiber over LAN)

| Check | Status |
|---|---|
| signal-receive.service | active, running 4h+, enabled, at boot |
| Daemon + JVM | alive, aarch64 natives loaded |
| `listContacts` RPC | returns `+4917620128275` uuid `dd58c93c-…` — account registered on Pi |
| SSE endpoint | `http://raspberrypi.local:8081/api/v1/events?account=...` connects (200, `text/event-stream`) |
| `/signal-send` RPC | returns SUCCESS; real message reached the account |
| `signal-pi.service` (raspberrypi) | **active, running**: `pi --print` with PI_SIGNAL env vars, SSE connected |

## How it works

```
Signal → signal-receive(:8081) → signal-pi(persistent listener on raspberrypi)
                                    → cyberkleiber:8081 (llama.cpp via Crossbar)
```

- `signal-receive.service` runs the signal-cli daemon bound to `0.0.0.0:8081` (port 8080 was occupied by `python3 -m http.server`, so signal-cli uses 8081). It's reachable via mDNS `raspberrypi.local`, bare hostname, or IP from all fleet machines.
- `signal-pi` = the persistent agent that consumes the SSE stream and forwards to llama.cpp at `http://cyberkleiber.fritz.box:8081` (provider `crossbar-llamacpp-cyberkleiber-fritz-box-8081`, baseUrl in `~/.pi/agent/crossbar.json`).
- The listener needs three env vars in its systemd unit environment: `PI_SIGNAL_ACCOUNT=<account>`, `PI_SIGNAL_DAEMON_URL=http://raspberrypi.local:8081`, `PI_SIGNAL_PRIMARY=true` (only the primary consumes messages; all other fleet instances stay silent).

## How it is implemented now (2026-11-13)

### `signal-pi` ExecStart — no wrapper script needed

The `@aalzubidy/pi-signal` extension (installed in `~/.pi/agent/settings.json`, package
`npm:@aalzubidy/pi-signal`) handles everything: it connects to the daemon's SSE endpoint on
`session_start`, injects every Note-to-Self into the agent session with
`pi.sendUserMessage(..., { deliverAs: followUp })`, and replies via the daemon JSON-RPC API
on `agent_end`. The only thing a persistent bot needs is **a running pi instance whose
extensions see the env vars**.

`pi --print` (no TTY → print mode) stays alive on its own: once the extension's open SSE
connection holds Node's event loop, a single invocation waits indefinitely for incoming
messages instead of exiting after its one-shot prompt. Responses go to stdout (the journal)
and are auto-sent back to Signal.

`PI_SIGNAL_PRIMARY=true` is required to activate the SSE listener; all other fleet instances
stay silent (they skip `startSSEListener`).

### Moving to chezmoi

`etc/systemd/system/signal-receive.service` + `etc/systemd/system/signal-pi.service` + a
hostname-gated install script, patterned after `run_once_5_aitools_7pi_memory_hub.sh`:

```ini
# signal-pi.service (raspberrypi only — see the run script's host gate)
[Service]
Type=simple
User=jan
Environment=HOME=/home/jan
Environment=USER=jan
Environment=PATH=/usr/local/bin:/usr/bin:/bin
Environment=PI_SIGNAL_ACCOUNT=+4917620128275
Environment=PI_SIGNAL_DAEMON_URL=http://raspberrypi.local:8081
Environment=PI_SIGNAL_PRIMARY=true
ExecStart=/home/jan/.local/share/mise/installs/pi/latest/pi/pi --print
Restart=on-failure
RestartSec=15
```

```bash
# .chezmoiscripts/run_once_5_aitools_8signal_pi.sh (hostname gate + both units)
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "⏭️ $(hostname) is not raspberrypi — signal-pi only runs there."
    exit 0
fi
# install both units, enabled --now (daemon-reload included)
sudo systemctl daemon-reload
sudo systemctl enable --now signal-receive.service signal-pi.service
```

### Verification (raspberrypi, 2026-11-13)

`cz apply --force` deployed both units. `systemctl --system status` now shows:

| Unit | State |
|---|---|
| `signal-receive.service` | active (running), enabled at boot, `-a +4917620128275 daemon --http 0.0.0.0:8081` |
| `signal-pi.service` | **active (running)** since 09:46, PID `pi --print`; `[signal] SSE connected — listening for incoming messages` in the journal |

The run script is idempotent: a second run reports both units "already installed and up to
date" — no `sudo`, no prompt, no restart.

### Still unverified (needs the phone)

- **Message reception + auto-reply**: the extension injects messages into the agent session
  and replies via the daemon JSON-RPC API, but this end-to-end path can only be tested by
  sending a Note-to-Self from the phone.
- **Conversation continuity**: `pi --print` sessions are one-shot; each injected message starts
  a new run in the same process. The session accumulates messages while the service lives,
  but a service restart starts a fresh session (no history).
- **`PI_SIGNAL_PRIMARY=false` on fleet peers** would be harmless: the extension skips the SSE
  listener on non-primary instances, so they stay silent and never consume the stream.

### Notes

- `~/.profile` no longer carries the Signal env vars (they were never verified there); the
  listener's are now in `signal-pi.service`'s `Environment=` lines.
- On **cyberkleiber**, `PI_SIGNAL_ACCOUNT` + `PI_SIGNAL_DAEMON_URL` still linger in the
  environment of `pi-memory-hub.service` (added manually, no pi instance runs there — so they
  are dead config; a `cz apply` there drops them). They do not affect anything until a
  `signal-pi` unit is deployed to that box, where `PI_SIGNAL_PRIMARY=false` would be needed.
- `.chezmoiscripts/run_once_5_aitools_8signal_pi.sh` is not affected by the
  `.chezmoiignore` rule for `.chezmoiscripts/8_*` (that pattern matches filenames starting
  with `8_`; this one starts with `run_once_5_aitools_`).
- The daemon unit's env vars (`PI_SIGNAL_ACCOUNT`, `PI_SIGNAL_QUIET_DAEMON`) serve the
  daemon loop, not the listener.

## Important traps (from the install)

- **signal-cli 0.14+ bundles native `libsignal_client` for amd64/mac/Windows only — no aarch64 Linux build.** The tarball's `libsignal-client-0.106.1.jar` extracts natives at runtime and fails with *"Missing required native library dependency: libsignal-client"*. Fix: download the matching community build `libsignal_jni.so-v0.106.1-aarch64-unknown-linux-gnu.tar.gz` from [exquo/signal-libs-build](https://github.com/exquo/signal-libs-build) and install to `/usr/lib/jni/` (already on Java's default `java.library.path`). Versions must match exactly.
- The daemon dies silently unless its output is visible: the service unit sets `PI_SIGNAL_QUIET_DAEMON=true`; set it false temporarily to see JVM failures in journalctl.
- **Account migration rule**: one host runs an account. Moving data (`~/.local/share/signal-cli/data`) requires stopping the old instance first, or SQLite DBs conflict.
- `PI_SIGNAL_*` env vars must be in the systemd unit `Environment=` for a persistent listener; `~/.profile` alone doesn't reach service-launched processes.
- Agent extension env constants are baked at session start — the running agent won't pick up new vars until restart (`/reload`).
