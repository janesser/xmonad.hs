#!/bin/bash

sudo apt install -y snap git

# asdf version manager
sudo snap install go --classic

go install github.com/asdf-vm/asdf/cmd/asdf@v0.20.0

export PATH=~/go/bin:$PATH # in case not yet set
fish -c "fish_add_path -Up ~/.asdf/shims/ ~/go/bin" # until next sourcing of .profile or asdf.fish

tee ~/.config/fish/conf.d/asdf.fish << EOF
# ASDF configuration code
if test -z \$ASDF_DATA_DIR
    set _asdf_shims "\$HOME/.asdf/shims"
else
    set _asdf_shims "\$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains \$_asdf_shims \$PATH
    set -gx --prepend PATH \$_asdf_shims
end
set --erase _asdf_shims
EOF

asdf completion fish > ~/.config/fish/completions/asdf.fish

# zellij has no upstream asdf plugin anymore (the plugin repos were removed from
# the asdf plugin index), so we ship a minimal local cargo plugin in the repo
# (asdf/zellij) and register it. Registering is idempotent and cheap; the heavy
# source build happens later in run_9_update_asdf.sh.
ASDF_PLUGIN_DIR="$ASDF_DATA_DIR/plugins/zellij"
[ -z "$ASDF_PLUGIN_DIR" ] && ASDF_PLUGIN_DIR="$HOME/.asdf/plugins/zellij"
mkdir -p "$(dirname "$ASDF_PLUGIN_DIR")"
cp -a "$CHEZMOI_SOURCE_DIR/asdf/zellij" "$ASDF_PLUGIN_DIR" || cp -a asdf/zellij "$ASDF_PLUGIN_DIR"
asdf plugin add zellij "$ASDF_PLUGIN_DIR" || true