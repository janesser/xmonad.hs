set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; 

fish_add_path -g $HOME/.cabal/bin $HOME/.ghcup/bin $PATH
