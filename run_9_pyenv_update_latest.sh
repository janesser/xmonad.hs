#!/bin/bash

LATEST_PYENV=`pyenv latest -k 3`
pyenv install -s $LATEST_PYENV
pyenv global $LATEST_PYENV
pip install --upgrade pip

# TODO uninstall non-system non-latest versions