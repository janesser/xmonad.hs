# WezTerm Proxy-to-GUI Connection Research

## Research Summary

Investigating how to route all local WezTerm usage through `wezterm-mux-server` and connect the GUI to it via `wezterm cli proxy`.

## Key Findings

### 1. `wezterm cli proxy` is NOT for human use
- It's an internal utility that talks to `wezterm-mux-server` via stdin/stdout
- Used by SSH domains and Unix domain proxy commands to connect to the remote multiplexer
- **Source**: [GitHub Discussion #1492](https://github.com/wezterm/wezterm/discussions/1492) (maintainer response)

### 2. Local WezTerm already uses embedded mux
- When you run `wezterm` locally, it embeds the mux server inside the GUI process
- The GUI process creates a socket file at: `/run/user/$UID/wezterm/gui-sock-$PID`
- **Source**: [Wez Furlong's note](https://fosstodon.org/@wez/112409663702870052)

### 3. Unix domains with `proxy_command`
- Can use `proxy_command` to tunnel to a mux server instead of direct socket connection
- Example for Docker: `proxy_command = { "docker", "exec", "-i", "container", "wezterm", "cli", "proxy" }`
- **Source**: [Discussion #5356](https://github.com/wezterm/wezterm/discussions/5356)

### 4. Default startup configuration
- `config.default_gui_startup_args = { "connect", "unix" }` makes wezterm connect to unix domain on startup
- `config.unix_domains = { { name = "unix" } }` defines the local unix domain
- **Source**: [Multiplexing docs](https://wezterm.org/multiplexing.html)

## The Problem

Attempting to use `wezterm cli proxy` to connect to a local GUI instance fails because:
- The local GUI embeds the mux server, not `wezterm-mux-server` standalone
- `wezterm cli proxy` expects to talk to a separate `wezterm-mux-server` daemon
- The expected socket file is missing because the architecture is different

## Next Steps

### 1. Create softlink to GUI socket file
```bash
# Find the running GUI PID and socket location
PID=$(pgrep -f wezterm-gui | head -1)
SOCKET="/run/user/$UID/wezterm/gui-sock-$PID"

# Create symlink in a known location
ln -sf "$SOCKET" ~/.local/share/wezterm/gui-sock
```

### 2. Configure unix_domains to use proxy_command
```lua
config.unix_domains = {
    {
        name = "gui",
        proxy_command = { "cat", "-", "/dev/null" },  -- placeholder
    },
}
```

### 3. Alternative: Use `wezterm cli` directly
Instead of `wezterm cli proxy`, try:
```bash
# Read socket path from environment or discover it
SOCKET=$WEZTERM_UNIX_SOCKET
wezterm cli --help  # Check available options
```

### 4. Check `WEZTERM_UNIX_SOCKET` environment variable
- This variable points to the running GUI's socket
- May need to set it explicitly or find it from the running process

### 5. Investigate `wezterm-mux-server` daemon options
```lua
config.daemon_options = {
    -- Check if we can force standalone mux server
}
```

### 6. Research socket discovery mechanism
- The `wezterm cli` tool uses `$WEZTERM_UNIX_SOCKET` or scans for running instances
- May need to set this variable to point to the GUI socket

## References

- [How does `wezterm cli proxy` work?](https://github.com/wezterm/wezterm/discussions/1492)
- [Feature Request: SSH tunnel to mux-server](https://github.com/wezterm/wezterm/issues/1568)
- [How to perform multiplex, within remote docker container?](https://github.com/wezterm/wezterm/discussions/5356)
- [Multiplexing docs](https://wezterm.org/multiplexing.html)
- [Unix domains config](https://wezterm.org/config/lua/config/unix_domains.html)
- [Understanding SSH ProxyCommand](https://www.cyberciti.biz/faq/linux-unix-ssh-proxycommand-passing-through-one-host-gateway-server/)
