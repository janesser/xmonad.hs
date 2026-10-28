# Guard: fish must be present for these files to have any effect.
command -v fish >/dev/null || exit 0
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; 

fish_add_path -g $HOME/.cabal/bin $HOME/.ghcup/bin $PATH
