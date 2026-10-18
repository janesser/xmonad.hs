#!/bin/bash

curl -fsSL https://herdr.dev/install.sh | sh

mkdir -p ~/.pi/agent/extensions
herdr integration install pi
