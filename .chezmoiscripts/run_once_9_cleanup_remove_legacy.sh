#!/bin/bash
# run_once_9_cleanup_remove_legacy.sh
#
# One-time cleanup for applications removed from the repo: purges leftover apt
# sources + keyrings and their apt packages (charm, glow, signal-desktop,
# element-desktop). Runs once (run_once) during `cz apply`.
#
# The snap migration (install snapd, then snap install signal-desktop /
# element-desktop) now lives in run_once_9_cleanup_snap_install.sh, which is
# ordered to run AFTER this script (alphabetically: "remove_legacy" < "snap_
# install") so the apt builds are purged before the snap equivalents are
# installed — avoiding both managers providing the same app at once.
#
# Idempotent: safe to re-run (`rm -f` and `apt remove --purge` are no-ops when
# nothing is present). Sudo is used only for commands in the scoped NOPASSWD
# sudoers drop-in (apt, rm).

sudo rm -f /etc/apt/sources.list.d/charm.sources
sudo rm -f /etc/apt/keyrings/charm.gpg
sudo apt remove --purge -y glow

sudo rm -f /etc/apt/sources.list.d/signal-xenial.sources
sudo rm -f /etc/apt/keyrings/signal-desktop-keyring.gpg
sudo apt remove --purge -y signal-desktop

sudo rm -f /etc/apt/sources.list.d/element-io.sources
sudo rm -f /etc/apt/keyrings/element-io-archive-keyring.gpg
sudo apt remove --purge -y element-desktop

sudo snap remove --purge glow # FIXME broken, gives permission denied error
