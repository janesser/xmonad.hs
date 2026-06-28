#!/bin/bash

# https://www.acervera.com/blog/2024/10/python-environment-poetry-pyenv-pipx/

sudo apt install -y libreadline-dev tk-dev
sudo apt remove -y pipx

rm -fR ~/.pyenv
curl -fsSL https://pyenv.run | bash

export PATH=~/.pyenv/bin:$PATH
eval "$(pyenv init - bash)"

LATEST_PYENV=`pyenv latest -k 3`
pyenv install -s $LATEST_PYENV
pyenv global $LATEST_PYENV

pip install --upgrade pip
pip install -U pipx
pipx ensurepath
pipx install -f poetry

curl -LsSf https://astral.sh/uv/install.sh | sh
