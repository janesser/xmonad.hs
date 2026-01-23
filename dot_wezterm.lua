local wezterm = require("wezterm")
local act = wezterm.action

local config = wezterm.config_builder()

config.keys = {
  {
    key = 'w',
    mods = 'CTRL|SHIFT',
    action = act.CloseCurrentPane { confirm = true },
  },
  {
    key = 'b',
    mods = 'CTRL',
    action = act.RotatePanes 'CounterClockwise',
  },
  {
    key = 'n',
    mods = 'CTRL',
    action = act.RotatePanes 'Clockwise'
  },
}


-- https://github.com/wezterm/wezterm/issues/7423 about ssh-agent confusion
local domains = wezterm.plugin.require("https://github.com/DavidRR-F/quick_domains.wezterm")
domains.apply_to_config(config)


return config
