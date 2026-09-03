#!/usr/bin/fish

# cleanup unmanaged files
for f in $(cz unmanaged ~/.config/fish/conf.d)
    if [ "$(basename $f)" != "asdf.fish" ]
        rm $f
    end
end

# update plugins
fisher update
