-- Keep only your personal keybinding overrides here. Add new bindings or
-- unbind defaults before replacing them.

-- See current bindings and descriptions:
--   omarchy menu keybindings --print

-- To disable every Omarchy default binding, set this in
-- ~/.config/hypr/hyprland.lua before require("default.hypr.omarchy"), then add
-- only the bindings you want below:
--   omarchy_default_bindings = false

-- To disable all preinstalled app/webapp bindings, set:
--   omarchy_preinstalled_bindings = false

-- Add a new binding.
-- o.bind("SUPER + SHIFT + R", "SSH", "alacritty -e ssh your-server")

-- Change an existing binding by unbinding it first, then binding the key again.
-- This example changes SUPER+SPACE from the launcher to the Omarchy root menu.
-- hl.unbind("SUPER + SPACE")
-- o.bind("SUPER + SPACE", "Omarchy menu", "omarchy-menu toggle root")

-- Disable a default binding without replacing it.
-- hl.unbind("SUPER + SHIFT + B")

-- Logitech MX Keys examples:
-- o.bind("SUPER + SHIFT + S", nil, "omarchy-capture-screenshot")
-- o.bind("SUPER + H", nil, "voxtype record toggle")
-- o.bind("SUPER + PERIOD", nil, "omarchy-shell shell toggle omarchy.emojis")

------------------------------------------------------------------------------------------

hl.unbind("SUPER + P")
o.bind("SUPER + P", "Omarchy menu", "omarchy-menu toggle root")

o.bind("SUPER + SHIFT + C", "Close window", hl.dsp.window.close())

o.bind("SUPER + SHIFT + COMMA", "Config (omarchy)", "code.omarchy")
o.bind("SUPER + SHIFT + PERIOD", "Config (chezmoi)", "code.chezmoi")

o.bind("SUPER + SHIFT + H", "Keybindings", "omarchy-menu-keybindings")
o.bind("SUPER + SHIFT + CTRL + H", "Keybindings (herdr)", "omarchy-menu-herdr-keybindings")

-- https://wiki.archlinux.de/title/XKB#Umlaute_(normale_Definition)
o.bind("CTRL + odiaeresis", "Clipboard manager", "omarchy-shell shell toggle omarchy.clipboard")

-- https://github.com/omacom/omarchy/blob/quattro/default/hypr/bindings/applications.lua

hl.unbind("SUPER + SHIFT + RETURN")
o.bind("SUPER + SHIFT + RETURN", "Terminal", "foot")
o.bind("SUPER + SHIFT + CTRL + RETURN", "Terminal", "foot herdr")

o.bind("SUPER + E", "File manager", "pcmanfm")
o.bind("SUPER + SHIFT + CTRL + RETURN", "Browser", { omarchy = "browser" })



