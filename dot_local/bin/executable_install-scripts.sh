#!/bin/sh

DIR="$(cd "$(dirname "$0")" && pwd)"

echo linking...
for S in $(ls ~/projs/xmonad.hs/.local/bin); do
    ln -sf $DIR/$S ~/.local/bin/$S
    echo $S linked.
done

echo detecting broken links...
find ~/.local/bin -xtype l
