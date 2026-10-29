-- Extra autostart processes.
-- o.launch_on_start("my-service")

o.launch_on_start("foot.btop")

o.launch_on_start("rencal")
o.launch_on_start("hylki")
o.launch_on_start("signal-desktop")
o.launch_on_start("omarchy-launch-webapp https://web.whatsapp.com/")

-- https://wiki.hypr.land/configuring/core/rules/window-rules/

-- $ hyprctl clients

hl.window_rule( { match = { class = "signal" }, workspace = "2 silent" } )
hl.window_rule( { match = { class = "co.hyprlab.Hylki" }, workspace = "2 silent" } )
hl.window_rule( { match = { class = "rencal" }, workspace = "2 silent" } )

hl.window_rule( { match = { class = "zen" }, workspace = 3 } )

hl.window_rule( { match = { class = "KeePassXC" }, workspace = "8 silent" } )

hl.window_rule( { match = { title = "web.whatsapp.com" }, workspace = "2 silent" } )
