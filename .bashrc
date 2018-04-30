# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# append to the history file, don't overwrite it
shopt -s histappend


# stack install cabal-install
PATH=~/.local/bin:$PATH

# go binaries
export GOPATH=~/.go
## openshift source2image
export PATH=$PATH:${GOPATH}/bin


# enable /usr/local/lib
export LD_LIBRARY_PATH=/usr/local/lib/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

# sqlplus
## https://help.ubuntu.com/community/Oracle%20Instant%20Client
export LD_LIBRARY_PATH=/usr/lib/oracle/12.2/client64/lib/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

# docker-compose
## https://github.com/palantir/docker-compose-rule/blob/master/docker-compose-rule-core/src/main/java/com/palantir/docker/compose/execution/DockerComposeExecutable.java
export DOCKER_COMPOSE_LOCATION=~/.local/bin/docker-compose
