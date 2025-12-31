local wezterm = require("wezterm")

local config = wezterm.config_builder()

local domains = wezterm.plugin.require("https://github.com/DavidRR-F/quick_domains.wezterm")
domains.apply_to_config(config)

local theme_rotator = wezterm.plugin.require 'https://github.com/koh-sh/wezterm-theme-rotator'
theme_rotator.apply_to_config(config)

return config
