if status is-interactive
    # Commands to run in interactive sessions can go here
end

status is-interactive; and begin
    set fish_tmux_autostart false
end

# Added by LM Studio CLI (lms)
set -gx PATH $PATH /home/jan/.lmstudio/bin
# End of LM Studio CLI section

