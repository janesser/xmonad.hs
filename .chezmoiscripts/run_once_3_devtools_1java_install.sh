#!/bin/bash

rm -rf ~/.sdkman
# remove from .bashrc

asdf plugin add java https://github.com/halcyon/asdf-java.git
asdf plugin add maven  https://github.com/halcyon/asdf-maven
asdf plugin add gradle https://github.com/rfrancis/asdf-gradle.git