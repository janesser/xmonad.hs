# SSHMUX Handover — Bob connects to Alice

## Architecture

```
Bob (Client)                                  Alice (Server)
┌──────────────────┐                         ┌──────────────────┐
│  wezterm GUI     │                         │  wezterm GUI     │
│  (local_gui)     │                         │  (local_gui)     │
│                  │                         │                  │
│  SSHMUX:Alice    │──── SSH ────►            │  SSHMUX:Alice    │
│  domain          │                         │  domain          │
│                  │                         │                  │
│  local_gui       │◄─── Unix Socket ──►     │  local_gui       │
│  domain          │                         │  domain          │
└──────────────────┘                         └──────────────────┘
```

- **Bob** spawns a tab via `wezterm cli spawn --domain-name SSHMUX:Alice`
- **Alice** runs `wezterm` as a multiplexer daemon
- Bob's GUI connects to Alice's multiplexer via the `local_gui` domain
- Both sides see the same WezTerm instance through the `local_gui` domain

---

## Server Alice (the multiplexer host)

### 1. Install WezTerm

```bash
# Alice must have a compatible version of wezterm installed
# (check version via `wezterm --version`)
```

### 2. Configure `~/.wezterm.lua` on Alice

```lua
-- === Alice's wezterm config ===

-- Listen for GUI connections from Bob
config.local_gui = {
  name = 'gui',
}

-- SSH domains — these auto-populate from ~/.ssh/config
config.ssh_domains = wezterm.default_ssh_domains()

-- Ensure Bob can connect to Alice's SSHMUX:Alice domain
-- The domain name "Alice" must match what Bob uses: SSHMUX:Alice
```

### 3. Ensure Bob can SSH to Alice

Alice's `~/.ssh/config` must have an entry for Bob:

```
# Alice's ~/.ssh/config
Host Bob
    HostName 192.168.1.100    -- Bob's IP or hostname
    User bob
    IdentityFile ~/.ssh/id_rsa
```

### 4. Start the wezterm multiplexer

On Alice, launch wezterm with the domain auto-connection:

```bash
# Start wezterm on Alice — it will auto-connect the local_gui domain
wezterm
```

Or explicitly:

```bash
wezterm connect local_gui
```

### 4b. Same-host: Start wezterm on the mux-server host

When the SSHMUX target is the same machine (e.g., WSL), the mux-server daemon runs on the remote host and the GUI connects via the unix socket:

```bash
# On the mux-server host, start wezterm with auto-connection
wezterm
```

The SSHMUX connection will find the domain by `name` and attach to the existing mux-server instance.

---

## Server Bob (the client)

### 1. Ensure SSH access to Alice

Bob's `~/.ssh/config` must have an entry for Alice:

```
# Bob's ~/.ssh/config
Host Alice
    HostName 192.168.1.101    -- Alice's IP or hostname
    User alice
    IdentityFile ~/.ssh/id_rsa
```

### 2. Spawn the SSHMUX:Alice domain

```bash
# From Bob's local wezterm GUI, spawn a new tab
wezterm cli spawn --domain-name SSHMUX:Alice
```

This will:
1. SSH to Alice (using the SSHMUX:Alice domain)
2. Connect to Alice's wezterm multiplexer daemon via Unix socket
3. The spawned tab's GUI auto-connects to Alice's `local_gui` domain

### 3. Verify the connection

After spawning, the new tab should show:
- Alice's shell prompt
- Bob's mouse, clipboard, and scrollback features working locally
- No SSH latency visible (multiplexing handles it)

---

## Domain Name Uniqueness (Critical)

**The `name` field in `config.ssh_domains` must be unique across ALL domain types** (local, unix, TLS, SSHMUX). You **cannot** have a local domain named `"local_gui"` and an SSHMUX domain also named `"local_gui"` — they would clash.

The domain name used in `SSHMUX:prefix` must match the `name` field in the server's `config.ssh_domains` table. If the server also uses a unix or TLS domain with that same `name`, that's the domain the SSHMUX connection will attach to.

---

## SSHMUX ↔ Local GUI on the Same Host

When SSHMUX connects to the **same machine** (e.g., WSL, or connecting back to a local host), the domain name alignment works differently from the remote Bob→Alice scenario:

The SSHMUX connection uses the `name` field to find the domain on the remote host. If the remote host runs a wezterm mux-server daemon with a `unix_domains` or `tls_domains` entry that shares that `name`, the SSHMUX connection attaches to that domain.

### WSL Example

```lua
-- WSL side (server)
config.unix_domains = {
  {
    name = 'wsl',
    socket_path = '/mnt/c/Users/USERNAME/.local/share/wezterm/sock',
    skip_permissions_check = true,
  },
}
```

```lua
-- Windows host side (client)
config.unix_domains = {
  {
    name = 'wsl',  -- MUST match WSL-side name
    serve_command = { 'wsl', 'wezterm-mux-server', '--daemonize' },
  },
}
config.default_gui_startup_args = { 'connect', 'wsl' }
```

Or via SSHMUX:

```bash
# On Windows, connect into WSL via SSH
wezterm connect SSHMUX:wsl
```

The key: `name = 'wsl'` is used consistently across all domain configs (unix on both sides, SSHMUX on client).

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "no such domain" error | Check that the server has `name = 'Alice'` in `config.ssh_domains`. The SSHMUX domain name must match exactly. |
| SSH connection refused | Verify the server's `~/.ssh/config` has the client entry and SSH access works |
| WezTerm not running on server | Launch `wezterm` on the server first |
| GUI not connecting to server | Ensure `local_gui` domain is configured on the server |
| Version mismatch | Both sides need the same or newer wezterm version (≥ 20230408-112425-69ae8472) |
| Domain name collision | The `name` field must be unique across ALL domain types (local, unix, TLS, SSHMUX). You can't have two domains with the same name. |
| Same-host mux-server | The SSHMUX connection finds the domain by `name` on the remote host. If the remote host uses `unix_domains` with the same `name`, the SSHMUX connection attaches there. |

---

## Common Variations

### Bob uses SSHMUX:Alice in a keybinding

```lua
-- Bob's ~/.wezterm.lua
local wezterm = require 'wezterm'

wezterm.on('spawn-domain', function(domain)
  wezterm.spawn(domain)
end)

-- Spawn SSHMUX:Alice in a new tab on a keybinding
```

### Alice uses a different hostname

If Alice's host is named differently in SSH config:

```bash
# Bob spawns using the SSHMUX prefix with the domain name
wezterm cli spawn --domain-name SSHMUX:Alice
```

The SSHMUX prefix matches the *domain name*, not the SSH config host name.

### Multiple SSHMUX domains

```bash
# Bob has multiple servers
wezterm cli spawn --domain-name SSHMUX:Alice
wezterm cli spawn --domain-name SSHMUX:Bob
wezterm cli spawn --domain-name SSHMUX:Charlie
```

Each requires its own entry in Alice's `config.ssh_domains`.

### SSHMUX auto-population from ~/.ssh/config

SSHMUX domains auto-populate from `~/.ssh/config`. Each populated host gets both a plain SSH domain (`SSH:my.server`) and a multiplexing domain (`SSHMUX:my.server`).

```bash
# Connect to a host auto-discovered from SSH config
wezterm connect SSHMUX:my.server
```

### SSHMUX `--attach` flag behavior

```bash
wezterm start --attach --domain SSHMUX:X
```

When connecting to an existing running instance, `--attach` should reuse existing panes instead of spawning a new tab. However, the `--attach` flag is **not passed through** `try_spawn()` in the CLI delegation path. Use `--always-new-process` to bypass delegation and force a new process:

```bash
wezterm start --always-new-process --attach --domain SSHMUX:X
```
