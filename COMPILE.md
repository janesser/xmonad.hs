# xmonad build from sources

    ln -sf ~/projs/xmonad.hs/.config/xmonad ~/.config/xmonad

Instructions here: <https://xmonad.org/INSTALL.html#build-using-stack>

    cd ~/projs/xmonad.hs/.config/xmonad
    git submodule add https://github.com/xmonad/xmonad
    git submodule add https://github.com/xmonad/xmonad-contrib
    git submodule foreach git reset --hard v0.18.0
    stack build

## setup with help of ghcup

Find ghcup here: <https://www.haskell.org/ghcup/>

    ghcup install stack recommended
    ghcup install ghcup recommend # 9.4.8 for now

## setup stack.yaml

Find LTS matching present ghc version

    ghc --version

<https://www.stackage.org/#about>

## Extras based on xmonad.hs

    sudo apt install libghc-split-dev libghc-hostname-dev
