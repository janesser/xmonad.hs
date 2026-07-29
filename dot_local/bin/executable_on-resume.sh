#!/bin/bash

echo I am $USER

killall -9 syndaemon
syndaemon "-i 1.2 -d -K -R" &

echo syndaemon restarted.