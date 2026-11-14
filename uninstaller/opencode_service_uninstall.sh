#!/bin/bash

systemctl --user stop opencode
systemctl --user disable opencode
rm ~/.config/systemd/user/opencode.service