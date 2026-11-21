#!/bin/bash

bash "$CHEZMOI_SOURCE_DIR/uninstaller/herdr_portfwd_uninstall.sh"
bash "$CHEZMOI_SOURCE_DIR/uninstaller/opencode_service_uninstall.sh"
bash "$CHEZMOI_SOURCE_DIR/uninstaller/signal_service_uninstall.sh"
bash "$CHEZMOI_SOURCE_DIR/uninstaller/sudoers_chezmoi_pi_uninstall.sh"
