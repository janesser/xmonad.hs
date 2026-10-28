#!/bin/bash
# run_once_5_aitools_4grom.sh
#
# Install (and keep up to date) grom — the btop-style terminal dashboard for
# Prometheus — from the upstream releases page.
#
# grom renders the llama.cpp metrics collected by Prometheus into a polished
# terminal dashboard. The dashboard config lives at
# ~/.config/grom/llama-metrics.yaml (managed by chezmoi); this script only
# handles the binary itself.
#
# POLICY: grom is a prebuilt upstream release, never built from source here.
# The release is downloaded over HTTPS and verified against the checksums file
# that github ships alongside it. The deployed version is stamped in
# ~/.local/share/grom_deployed_version, so a `cz update` only re-downloads when
# the latest release advances.
#
# Root usage: `install` to ~/.local/bin is user-owned and needs NO sudo.

set -o pipefail

GROM_VERSION="$(curl -s https://api.github.com/repos/qf-studio/grom/releases/latest | python3 -c 'import json,sys;print(json.load(sys.stdin)["tag_name"])' 2>/dev/null)"
if [ -z "$GROM_VERSION" ]; then
    echo "$(basename "$0"): ERROR: could not determine the latest grom release (check network)."
    exit 1
fi
# Strip the v prefix (v0.3.0 -> 0.3.0).
GROM_VERSION="${GROM_VERSION#v}"
GROM_RELEASE="https://github.com/qf-studio/grom/releases/download/v${GROM_VERSION}/grom_${GROM_VERSION}_linux_amd64.tar.gz"
GROM_CHECKSUMS="https://github.com/qf-studio/grom/releases/download/v${GROM_VERSION}/checksums.txt"
# Stamp of the release version currently installed at ~/.local/bin/grom.
GROM_STAMP="$HOME/.local/share/grom_deployed_version"

log() { echo "$(basename "$0"): $*"; }

ensure_grom() {
    local local_version
    local_version="$(cat "$GROM_STAMP" 2>/dev/null)"

    if [ "$local_version" = "$GROM_VERSION" ] && [ -x "$HOME/.local/bin/grom" ]; then
        log "grom $GROM_VERSION already installed; reusing."
        return 0
    fi

    log "Downloading grom $GROM_VERSION from $GROM_RELEASE"
    local tmpdir
    tmpdir="$(mktemp -d)"
    if ! curl -sL -o "$tmpdir/grom.tar.gz" "$GROM_RELEASE"; then
        log "ERROR: could not download grom $GROM_VERSION (check network)."
        rm -rf "$tmpdir"
        return 1
    fi

    # Verify the tarball against the upstream checksums file.
    if ! curl -sL -o "$tmpdir/checksums.txt" "$GROM_CHECKSUMS"; then
        log "WARNING: could not download checksums.txt — skipping verification."
    else
        local expected
        expected="$(grep "grom_${GROM_VERSION}_linux_amd64.tar.gz" "$tmpdir/checksums.txt" | awk '{print $1}')"
        if [ -n "$expected" ]; then
            local actual
            actual="$(sha256sum "$tmpdir/grom.tar.gz" | awk '{print $1}')"
            if [ "$actual" != "$expected" ]; then
                log "ERROR: checksum mismatch (got $actual, want $expected)."
                rm -rf "$tmpdir"
                return 1
            fi
            log "checksum verified."
        else
            log "WARNING: no grom checksum found in checksums.txt."
        fi
    fi

    mkdir -p "$tmpdir/grom"
    if ! tar -xzf "$tmpdir/grom.tar.gz" -C "$tmpdir/grom"; then
        log "ERROR: could not extract grom $GROM_VERSION."
        rm -rf "$tmpdir"
        return 1
    fi

    if [ ! -x "$tmpdir/grom/grom" ]; then
        log "ERROR: extracted grom $GROM_VERSION is not an executable."
        rm -rf "$tmpdir"
        return 1
    fi

    if ! install -m755 "$tmpdir/grom/grom" "$HOME/.local/bin/grom"; then
        log "ERROR: could not install grom to ~/.local/bin/grom."
        rm -rf "$tmpdir"
        return 1
    fi

    printf '%s' "$GROM_VERSION" > "$GROM_STAMP"
    log "installed grom $GROM_VERSION to ~/.local/bin/grom."
    rm -rf "$tmpdir"
    return 0
}

ensure_grom || log "grom install was not applied — see errors above; re-run cz update."

log "Done. The llama-metrics dashboard is at ~/.config/grom/llama-metrics.yaml (grom run --config ~/.config/grom/llama-metrics.yaml)."
