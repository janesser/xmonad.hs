# ~/.config/fish/conf.d/zellij.fish
#
# Persistent "always the same session" zellij setup.
#
# On every interactive login -- local console OR ssh -- we attach to a single
# named session, "persistent", creating it on the first run. Because that
# session is serialized to disk (see ~/.config/zellij/config.kdl ->
# session_serialization true), it survives ssh disconnects and is resurrected
# across reboots, so you always land back in the exact same workspace.
#
# This is zellij's own auto-start snippet (see
# `zellij setup --generate-auto-start fish`), with an explicit session name
# injected so the session is deterministic instead of zellij's default
# "zellij-autostart".
#
# Auto-attach is DISABLED by default: opening a shell just gives a normal
# shell. To use zellij, attach by hand:
#     zellij attach persistent
#
# Re-enable by setting ZELLIJ_AUTO_ATTACH below to true (and removing the
# guard in the block above).

if status is-interactive
    set -g ZELLIJ_SESSION_NAME persistent
    set -g ZELLIJ_AUTO_ATTACH false
    set -g ZELLIJ_AUTO_EXIT false

    # Avoid re-attaching if we are already inside a zellij session.
    if not set -q ZELLIJ
        # ZELLIJ_AUTO_ATTACH is false, so we intentionally do NOT launch
        # zellij here -- a plain shell is started instead.
        if test "$ZELLIJ_AUTO_ATTACH" = "true"
            zellij attach -c persistent
        end
    end
end
