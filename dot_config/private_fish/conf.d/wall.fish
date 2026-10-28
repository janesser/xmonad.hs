# ~/.config/fish/conf.d/wall.fish
#
# Pane-visible "wall" banner (see discoveries/wall-fix.md).
#
# The system `wall` can't reach zellij panes: /run/utmp is empty and zellij
# owns the terminal ptys. Instead, ~/.local/bin/wall posts a message to a shared
# store, and each interactive fish prints any not-yet-seen messages as a banner
# above its prompt on the next prompt draw.
#
# Each pane is its own fish process, so each pane remembers the last message id
# it has shown (in $WALL_STORE/seen/<fish_pid>) and shows each message exactly
# once, on its next prompt. Prompt-gated, not a live push — fine for
# announcements (like a MOTD per pane). No zellij or utmp involvement.

# Resolve the store once (allow env override, then XDG, then the default).
if not set -q WALL_STORE
    if set -q XDG_DATA_HOME
        set -g WALL_STORE "$XDG_DATA_HOME/wall"
    else
        set -g WALL_STORE "$HOME/.local/share/wall"
    end
end
set -g WALL_INBOX "$WALL_STORE/inbox"
set -g WALL_LATEST "$WALL_STORE/latest"
set -g WALL_SEEN "$WALL_STORE/seen"

# Convert a nanosecond-epoch id into a human local timestamp.
function _wall_when
    set --local secs (string sub --start 1 --length 10 "$argv[1]")
    date -d "@$secs" '+%Y-%m-%d %H:%M:%S' 2>/dev/null || echo "$argv[1]"
end

function _wall_render_banner
    status is-command-substitution && return
    # Cheap early-out: nothing has been posted yet.
    test -f "$WALL_LATEST" || return
    # NB: read files with `cat`, never a `<` redirect inside $(...) inside a
    # block -- that redirect silently yields empty inside if/function bodies on
    # this fish, so a read would fall back to the uninitialised value.
    set --local latest_id (string trim -- (cat "$WALL_LATEST"))
    test -n "$latest_id" || return

    # This pane's last-shown id (0 = everything is new).
    set --local pid $fish_pid
    set --local shown 0
    if test -f "$WALL_SEEN/$pid"
        # bare `set` (not `set --local`): inside an `if`, `--local` would create
        # an if-block-local that shadows the function variable, so `shown` would
        # revert to 0 after the block and every pane would reprint forever.
        set shown (string trim -- (cat "$WALL_SEEN/$pid"))
    end

    # Every inbox message whose id is newer than `shown`, in ascending order.
    # Nanosecond-epoch ids are 19-digit integers that fit in i64, so `test -gt`
    # (integer compare) is correct and avoids `math`, which rejects `>`.
    set --local to_show
    for d in "$WALL_INBOX"/*
        test -d "$d" || continue
        set --local id (basename "$d")
        if test "$id" -gt "$shown"
            test -f "$d/body" || continue
            set --append to_show "$id"
            set shown $id
        end
    end

    test (count $to_show) -eq 0 && return

    # Banner rules: width caps so it never overflows a wide pane. COLUMNS is
    # always set by an interactive fish, but default so a stray call can't error.
    set --local cols $COLUMNS
    test -n "$cols" || set cols 80
    test $cols -lt 24 && set cols 24
    test $cols -gt 78 && set cols 78
    set --local rule (string repeat --count $cols '=')

    for id in $to_show
        set --local body (cat "$WALL_INBOX/$id/body")
        printf '\n%s\nwall · %s\n\n%s\n%s\n' \
            "$rule" (_wall_when "$id") "$body" "$rule"
    end

    # Remember the newest id this pane has seen so we don't reprint it.
    mkdir -p "$WALL_SEEN"
    printf '%s' "$shown" > "$WALL_SEEN/$pid"
end

# Compose with any pre-existing fish_prompt (e.g. a theme or kitty shell
# integration): prepend the banner, never replace the original.
#
# Wrapped only once per fish session, via a sentinel, so re-sourcing the file
# can't copy our own wrapper into _wall_prompt_orig (which would make it call
# itself and recurse). We also define the wrapper when fish has no fish_prompt
# of its own (the default), so the banner shows regardless.
if not set -q __wall_prompt_active
    # `functions --copy` refuses to overwrite an existing destination, so clear
    # any leftover _wall_prompt_orig first. The sentinel keeps this block running
    # only once per session, so fish_prompt here is always the original (never
    # our own wrapper) -> no recursion on re-source.
    if functions -q _wall_prompt_orig
        functions --erase _wall_prompt_orig
    end
    if functions -q fish_prompt
        functions --copy fish_prompt _wall_prompt_orig
    end
    function fish_prompt
        _wall_render_banner
        functions -q _wall_prompt_orig && _wall_prompt_orig
    end
    set -g __wall_prompt_active yes
end
