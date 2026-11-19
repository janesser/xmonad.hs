#!/bin/bash
# run_5_signal_update.sh — runs on every `chezmoi apply` (see the run_once_*
# install scripts). Checks whether a newer signal-cli release exists and applies
# it. Nothing happens if the installed version is already the latest, so this
# stays cheap: one HEAD request against GitHub per run.

set -uo pipefail

CHEZMOI_SOURCE_DIR="${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}"

log_info() { echo "[INFO] $*"; }
log_ok()   { echo "[OK]   $*"; }
log_warn() { echo "[WARN] $*"; }
log_error(){ echo "[ERROR] $*"; }

# signal-cli per-user install (see run_once_5_aitools_8signal_pi.sh)
SIGNAL_CLI_LINK="$HOME/.local/bin/signal-cli"
SIGNAL_CLI_DIR="$HOME/.local/lib/signal-cli"

if ! command -v signal-cli &>/dev/null; then
    log_warn "signal-cli not installed — this update script does nothing until install."
    exit 0
fi

VERSION_INSTALLED=$(signal-cli --version 2>/dev/null | grep -oE '[0-9.]+' | head -1 || true)
[ -z "$VERSION_INSTALLED" ] && { log_warn "Could not resolve the installed signal-cli version."; exit 0; }

# Latest release tag from GitHub (e.g. v0.14.10 -> 0.14.10).
VERSION_LATEST=$(curl -sL -o /dev/null -w '%{url_effective}' \
    https://github.com/AsamK/signal-cli/releases/latest 2>/dev/null | sed 's|.*/v||' || true)
[ -z "$VERSION_LATEST" ] && { log_warn "Could not check the latest signal-cli release."; exit 0; }

# Source the shared release-tag validation helper (commons is a source tree, ignored by
# chezmoi rendering — see .chezmoiignore). The tag always carries the v prefix once
# normalized (v0.14.10) so it can be used directly as a URL path component.
source "$CHEZMOI_SOURCE_DIR/commons/validate_release_tag.sh"
RELEASE_TAG="$(normalize_release_tag "$VERSION_LATEST")"

# Fail-closed release-version guard: $VERSION_LATEST comes from a GitHub response (an
# untrusted source for foxguard's taint engine), so before the tag is interpolated into
# the download URL / tarball filename below, it must be an exact GitHub release tag or be
# rejected. This is also the fail-closed allowlist shape foxguard's taint engine
# recognizes as a sanitizer for bash/taint-ssrf — the flow clears here by design.
if [[ ! "$RELEASE_TAG" =~ ^v[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z][0-9A-Za-z.-]*)?$ ]]; then
    log_error "Invalid signal-cli version tag: $RELEASE_TAG"
    exit 1
fi

if [ "$VERSION_INSTALLED" = "${RELEASE_TAG#v}" ]; then
    echo "[INFO] signal-cli is up to date: $VERSION_INSTALLED == ${RELEASE_TAG#v}"
    exit 0
fi

log_info "signal-cli update available: $VERSION_INSTALLED -> ${RELEASE_TAG#v}"

TARBALL="/tmp/signal-cli-${RELEASE_TAG#v}.tar.gz"
curl -L -o "$TARBALL" \
    "https://github.com/AsamK/signal-cli/releases/download/${RELEASE_TAG}/signal-cli-${RELEASE_TAG#v}.tar.gz"
[ -f "$TARBALL" ] || { log_error "Download of $TARBALL failed."; exit 1; }

# `tar xf` needs root: the target dir may still be root-owned from the stale
# /opt install (a per-user layout is not guaranteed to exist yet), so a plain
# untar would fail for jan. Without this message, sudo's prompt looks unexplained.
if [ -d "$SIGNAL_CLI_DIR/signal-cli-${VERSION_LATEST}" ]; then
    log_ok "${VERSION_LATEST} already extracted per-user -- skipped."
else
    sudo tar xf "$TARBALL" -C "$SIGNAL_CLI_DIR"
fi
mkdir -p "$HOME/.local/bin"
rm -f "$SIGNAL_CLI_LINK"
ln -sf "${SIGNAL_CLI_DIR}/signal-cli-${VERSION_LATEST}/bin/signal-cli" "$SIGNAL_CLI_LINK"
# only clean up when the tarball is still there (a re-run leaves it gone)
if [ -f "$TARBALL" ]; then
    sudo rm -f "$TARBALL"
fi

log_ok "signal-cli updated to $VERSION_LATEST: ${SIGNAL_CLI_DIR}/signal-cli-${VERSION_LATEST}"
echo "[INFO] restarting signal-receive.service to use the new daemon..."
systemctl --user restart signal-receive.service 2>/dev/null || \
    log_warn "(could not restart signal-receive.service — it may need a login shell)"
