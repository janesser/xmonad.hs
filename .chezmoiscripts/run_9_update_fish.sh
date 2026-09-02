#!/usr/bin/fish

chezmoi add ~/.config/fish/conf.d/asdf.fish

# cleanup unmanaged files
for f in $(cz unmanaged ~/.config/fish/conf.d)
    rm $f
end

# update plugins
fisher update
