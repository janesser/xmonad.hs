#!/bin/bash

sudo apt install -y snap git

# asdf version manager
sudo snap install go --classic

go install github.com/asdf-vm/asdf/cmd/asdf@v0.20.0

tee ~/.config/fish/conf.d/asdf.fish << EOF
# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims
EOF

asdf completion fish > ~/.config/fish/completions/asdf.fish
