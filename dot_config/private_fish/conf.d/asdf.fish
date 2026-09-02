# ASDF configuration code
if test -z 
    set _asdf_shims "/home/jan/.asdf/shims"
else
    set _asdf_shims "/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains  /home/jan/go/bin:/home/jan/.ghcup/bin:/home/jan/.nvm/versions/node/v24.20.0/bin:/home/jan/.cargo/bin:/home/jan/.local/bin:/usr/share/safe-rm/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/sbin
    set -gx --prepend PATH 
end
set --erase _asdf_shims
