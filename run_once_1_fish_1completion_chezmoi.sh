#!/bin/bash

mkdir -p ~/.config/fish/completions
chezmoi completion fish | tee  ~/.config/fish/completions/chezmoi.fish