#!/bin/bash

rm -rf ~/.sdkman
rm -f ~/.config/fish/completions/sdk.fish
rm -f ~/.config/fish/conf.d/sdk.fish
# removed sdk from .bashrc

export PATH=~/go/bin:$PATH # in case not yet set

asdf plugin add java https://github.com/halcyon/asdf-java.git
asdf plugin add maven  https://github.com/halcyon/asdf-maven
asdf plugin add gradle https://github.com/rfrancis/asdf-gradle.git