#!/bin/bash

asdf plugin add kubectl https://github.com/asdf-community/asdf-kubectl.git
asdf install kubectl latest

asdf plugin add helm https://github.com/Antiarchitect/asdf-helm.git
asdf install helm latest