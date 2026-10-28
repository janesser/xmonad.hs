# Guard: fish must be present for these files to have any effect.
command -v fish >/dev/null || exit 0
if status is-interactive
    # Commands to run in interactive sessions can go here
end
