#!/bin/bash

sudo systemctl disable on-resume.service

sudo rm /etc/systemd/system/on-resume.service

sudo systemctl daemon-reload