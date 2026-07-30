# Filesystem Mappings — Chuwi UBook XPro

Softlinks in `agent_work/` to directories with spaces in the original path.

## Table

| # | Original Path (with spaces) | Softlink Name | Notes |
|---|---|---|---|
| 1 | `chuwi-ubook-xpro/Display adapters` | `Display_adapters` | |
| 2 | `chuwi-ubook-xpro/Human Interface Devices` | `Human_Interface_Devices` | |
| 3 | `chuwi-ubook-xpro/Network adapters` | `Network_adapters` | |
| 4 | `chuwi-ubook-xpro/Software components` | `Software_components` | |
| 5 | `chuwi-ubook-xpro/Sound, video and game controllers` | `Sound_video_and_game_controllers` | Contains comma |
| 6 | `chuwi-ubook-xpro/System devices` | `System_devices` | |

## Base paths

- **Source**: `/home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers/chuwi-ubook-xpro/`
- **Links**: `/home/jan/.local/share/chezmoi/devices/chuwi_ubook_xpro/chuwi_camera_drivers/agent_work/`

## Naming convention

Spaces → underscores (` ` → `_`)
Commas → underscores (`, ` → `_`)
