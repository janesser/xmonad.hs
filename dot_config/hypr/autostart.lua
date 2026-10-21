-- Extra autostart processes.
-- o.launch_on_start("my-service")

o.launch_on_start("foot btop")
o.launch_on_start("rencal")
o.launch_on_start("hylki")

o.launch_on_start("signal-desktop")

hl.window_rule( { match = { class = "signal-desktop" }, workspace = 2 } )

-- https://wiki.hypr.land/configuring/core/rules/window-rules/