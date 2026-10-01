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

if status is-interactive
    set -g ZELLIJ_SESSION_NAME persistent
    set -g ZELLIJ_AUTO_ATTACH true
    set -g ZELLIJ_AUTO_EXIT true

    # Avoid re-attaching if we are already inside a zellij session.
    if not set -q ZELLIJ
        if test "$ZELLIJ_AUTO_ATTACH" = "true"
            zellij attach -c persistent
        else
            zellij
        end

        # Detach/exit cleanly so closing or detaching from zellij drops the
        # shell (and the ssh connection) instead of leaving a dangling prompt.
        if test "$ZELLIJ_AUTO_EXIT" = "true"
            kill $fish_pid
        end
    end
end
