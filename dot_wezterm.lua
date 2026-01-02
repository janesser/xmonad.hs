local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.keys = {
  {
    key = 'w',
    mods = 'CTRL',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
}


-- https://github.com/wezterm/wezterm/issues/7423 about ssh-agent confusion
local domains = wezterm.plugin.require("https://github.com/DavidRR-F/quick_domains.wezterm")
domains.apply_to_config(config)


return config
