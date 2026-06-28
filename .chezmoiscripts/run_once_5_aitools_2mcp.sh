#!/bin/bash

sudo snap install task --classic

cd ~/projs
git clone https://github.com/stacklok/toolhive.git
cd toolhive

task install