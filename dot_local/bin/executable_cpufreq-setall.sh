#!/bin/bash

echo $1 | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor