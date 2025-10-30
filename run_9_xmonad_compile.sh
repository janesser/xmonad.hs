#!/bin/bash

if command -v stack > /dev/null &2>1
then
    stack build --stack-yaml ~/.config/xmonad/stack.yaml
else
    echo stack is not \(yet\) on PATH=$PATH.
fi