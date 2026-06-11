#!/usr/bin/fish

pyenv update

set LATEST_PYENV "$(pyenv latest -k 3)"
pyenv install -s $LATEST_PYENV
pyenv global $LATEST_PYENV
pyenv init - fish | source
pip install --upgrade pip

# uninstall non-system non-latest versions
for v in $(pyenv versions | grep -v system | grep -v $LATEST_PYENV);
    pyenv uninstall $v
end

return 0