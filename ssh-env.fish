#!/usr/bin/fish

## ln -sf ./ssh-env.fish ~/.config/fish/conf.d/ssh-env.fish

if test -e ~/.ssh/env
    source ~/.ssh/env
end
