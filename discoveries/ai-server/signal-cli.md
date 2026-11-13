# Signal CLI Integration — State & Handover

## Summary

signal-cli 0.14.10 is installed and running on **raspberrypi** with the same account, but the integration is not yet in chezmoi. Two units are planned (`signal-receive` + `signal-pi`), both hostname-gated to raspberrypi. The env vars that make a listener consume messages are only set via `~/.profile` edits — they need to move into systemd unit Environment lines.

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

## How it works

```
Signal → signal-receive(:8081) → signal-pi(persistent listener on raspberrypi)
                                    → cyberkleiber:8081 (llama.cpp via Crossbar)
```

- `signal-receive.service` runs the signal-cli daemon bound to `0.0.0.0:8081` (port 8080 was occupied by `python3 -m http.server`, so signal-cli uses 8081). It's reachable via mDNS `raspberrypi.local`, bare hostname, or IP from all fleet machines.
- `signal-pi` = the persistent agent that consumes the SSE stream and forwards to llama.cpp at `http://cyberkleiber.fritz.box:8081` (provider `crossbar-llamacpp-cyberkleiber-fritz-box-8081`, baseUrl in `~/.pi/agent/crossbar.json`).
- The listener needs three env vars in its systemd unit environment: `PI_SIGNAL_ACCOUNT=<account>`, `PI_SIGNAL_DAEMON_URL=http://raspberrypi.local:8081`, `PI_SIGNAL_PRIMARY=true` (only the primary consumes messages; all other fleet instances stay silent).

## Open items (needs deciding)

1. **`signal-pi` ExecStart**: no native bot mode in pi — only `--print` (non-interactive, one-shot). A persistent listener must loop: open SSE → `pi --print <message>` with provider/model flags → send reply via daemon JSON-RPC. Draft unit:

```ini
[Unit]
Description=Signal <-> pi persistent agent — SSE listener + llama.cpp bridge
After=network-online.target signal-receive.service
Wants=network-online.target signal-receive.service

[Service]
Type=simple
User=jan
Environment=HOME=/home/jan
Environment=PATH=/usr/local/bin:/usr/bin:/bin
Environment=PI_SIGNAL_ACCOUNT=<account>
Environment=PI_SIGNAL_DAEMON_URL=http://raspberrypi.local:8081
Environment=PI_SIGNAL_PRIMARY=true
ExecStart=/usr/local/bin/signal-pi-listener.sh
Restart=on-failure
RestartSec=15

[Install]
WantedBy=multi-user.target
```

2. **Move to chezmoi**: create `etc/systemd/system/signal-receive.service` and `signal-pi.service` in the source repo, plus `.chezmoiscripts/run_once_5_aitools_signal_pi.sh` (hostname gate). Pattern from `run_once_5_aitools_7pi_memory_hub.sh`:

```bash
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "⏭️ $(hostname) is not raspberrypi — signal-pi only runs there."
    exit 0
fi
sudo install -o root -g root -m 0644 etc/systemd/system/signal-pi.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now signal-pi.service
```

## Important traps (from the install)

- **signal-cli 0.14+ bundles native `libsignal_client` for amd64/mac/Windows only — no aarch64 Linux build.** The tarball's `libsignal-client-0.106.1.jar` extracts natives at runtime and fails with *"Missing required native library dependency: libsignal-client"*. Fix: download the matching community build `libsignal_jni.so-v0.106.1-aarch64-unknown-linux-gnu.tar.gz` from [exquo/signal-libs-build](https://github.com/exquo/signal-libs-build) and install to `/usr/lib/jni/` (already on Java's default `java.library.path`). Versions must match exactly.
- The daemon dies silently unless its output is visible: the service unit sets `PI_SIGNAL_QUIET_DAEMON=true`; set it false temporarily to see JVM failures in journalctl.
- **Account migration rule**: one host runs an account. Moving data (`~/.local/share/signal-cli/data`) requires stopping the old instance first, or SQLite DBs conflict.
- `PI_SIGNAL_*` env vars must be in the systemd unit `Environment=` for a persistent listener; `~/.profile` alone doesn't reach service-launched processes.
- Agent extension env constants are baked at session start — the running agent won't pick up new vars until restart (`/reload`).
