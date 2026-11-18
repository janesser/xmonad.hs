#!/usr/bin/env bash
# executable_build-lan-repo-common.sh — shared helpers for the build-lan-repo script family.
#
# Sourced at the top of every member (entry point, build stage, publish stage). It owns no mode
# decisions or invocation logic: only validated defaults, logging and the package resolution
# functions the stages share. The family's files are named with a shared prefix so they stay
# discoverable as one system, while each file stays small enough to read in full.
#
# Import at the top of a family member:
#   SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
#   source "$SCRIPT_DIR/build-lan-repo-common.sh"

set -uo pipefail

# --- logging ----------------------------------------------------------------------
log() { echo "=> $(basename "$0"): $*" >&2; }

# --- validated environment defaults (a caller may override everything) ---------------
BUILD_DIR="${BUILD_DIR:-/media/sailor/lan-repo/builds}"
REPO_DIR="$BUILD_DIR/repo"
PI_HOST="${PI_HOST:-f3nt0t9dnh9t8l0v.myfritz.net}"
PI_USER="jan"
PI_DIR="${PI_DIR:-$HOME/.local/arch-repo}"
MARKER_DIR="${MARKER_DIR:-$BUILD_DIR/.last-built}"
NPROC="$(nproc 2>/dev/null || echo 1)"
LIST="${LAN_REPO_LIST:-$HOME/.local/build-lan-repo/packages.list}"

# --- hostname guard: only meaningful on the fleet build hosts -----------------------
if [ "${FAMILY_GUARD_DONE:-}" != "1" ]; then
    FAMILY_GUARD_DONE=1
    if [ "$(hostname)" != "cyberkleiber" ] && [ "$(hostname)" != "lincopta" ]; then
        log "only intended for the fleet build hosts (cyberkleiber/lincopta); run on the build host, not on $(hostname)"
        exit 1
    fi
fi

# --- git clone over https (bypassing host-level ssh rewrites) -----------------------
# The host's ~/.config/git/config rewrites `https://github.com/janesser/*` to
# `git@github.com:janesser/` — agent-backed, unavailable without a forwarded agent.
# Override the rewrite for github.com and keep the clone on plain https (no key, no
# agent needed).
git_https_clone() { git -c 'url."https://github.com/janesser/".insteadOf=' clone "$@"; }

# --- validate one package name ------------------------------------------------------
# Package names come from packages.list or the command line: they must be safe path
# components (no /, .., spaces) before being used in a URL or a file path.
is_safe_name() { [[ "$1" =~ ^[a-zA-Z0-9._-]+$ ]]; }

# --- the name a package publishes as --------------------------------------------------
# AUR names are valid path components; packages that already end in -bin publish as-is.
# For the others the published artifact is named after the PKGBUILD's pkgname (which may
# be <aur> or <aur>-bin), so the script resolves it from the cloned PKGBUILD.
pkgname_from_pkbuild() {
    local pkbuild="$(pkbuild_path "$1")"
    [ -f "$pkbuild" ] || return 1
    grep -m1 '^pkgname=' "$pkbuild" \
        | sed 's/^pkgname=[[:space:]]*//' \
        | sed 's/[()]//g' \
        | sed 's/"//g' \
        | sed "s/'//g" \
        | awk '{print $1}'
}

published_name() {
    local aur="$1"
    if [[ "$aur" == *"-bin" ]]; then
        printf '%s\n' "$aur"
    else
        pkgname_from_pkbuild "$aur" || printf '%s\n' "${aur}-bin"
    fi
}

# --- the AUR name of a published artifact (fallback for leftovers) -------------------
# hylki-bin -> hylki (the AUR package), hylki -> hylki.
aur_name() {
    local pub="$1"
    if [[ "$pub" == *"-bin" ]]; then
        printf '%s\n' "${pub%-bin}"
    else
        printf '%s\n' "$pub"
    fi
}

# --- the build dir and PKGBUILD of one package --------------------------------------
pkbuild_dir() { printf '%s\n' "$BUILD_DIR/$1"; }
pkbuild_path() { printf '%s\n' "$BUILD_DIR/$1/PKGBUILD"; }

# --- get the PKGBUILD for one package -----------------------------------------------
# Always clone the current AUR PKGBUILD: a stale local one (left from an earlier,
# possibly incomplete run) would otherwise decide the version that gets built instead
# of the one upstream published. Removing $BUILD_DIR/<pkg> first also wipes leftover
# state so makepkg does not refuse to rebuild over it.
get_pkbuild() {
    local aur="$1"
    # validate the package name before it touches any path or URL (no /, .., spaces)
    is_safe_name "$aur" || return 1
    # wipe stale state so makepkg does not refuse to rebuild over it; jan owns $BUILD_DIR
    # and everything in it (set by the pre-run ownership fix), so `rm -rf` does the job.
    # When a tree is still owned by someone else, restore jan's ownership through a rootful
    # container run before removing it: `sudo` requires a TTY, which non-interactive runs
    # (the systemd rebuild trigger, an agent run) do not have — that is what made the stale
    # package dir block the AUR clone. A tree that does not exist yet needs no cleanup.
    p=$(printf '%q' "$(pkbuild_dir "$aur")")
    if [ -d "$p" ] && ! [ -w "$p" ]; then
        podman run --rm -v "$BUILD_DIR:/builds" archlinux:base-devel \
            bash -c "chmod -R u+w '$p' && chown -R 0:0 '$p'" || {
                log "ERROR: could not restore ownership of $p"; return 1;
            }
    fi
    # $p is pkbuild_dir($aur) — resolved under the fixed base $BUILD_DIR/src/; $aur is
    # gated to a safe path component (no /, .., spaces) by is_safe_name() above, so it
    # cannot traverse outside that directory.
    rm -rf "$p" || { log "ERROR: could not remove $p"; return 1; }
    echo "=> $0: cloning the $aur PKGBUILD" >&2
    # packages that do not live on the AUR (a fork on github.com) are fetched from there:
    # signal-tauri-bin is a cargo package with no AUR entry — its PKGBUILD lives in
    # https://github.com/janesser/signal-tauri.git, so clone that instead of AUR. The
    # full history is required: the version this script computes from it (<cargo>.r<count>
    # .g<h>) must match exactly what makepkg builds (its pkgver() reads the same repo),
    # and a shallow clone always reports rev-list --count HEAD as 1 — which would never
    # match the real artifact version.
    if [ "$aur" = "signal-tauri-bin" ]; then
        # plain https (the host's git config would otherwise rewrite this to ssh)
        git_https_clone "https://github.com/janesser/signal-tauri.git" "$(pkbuild_dir "$aur")" || {
            echo "=> $0: ERROR: could not clone https://github.com/janesser/signal-tauri.git" >&2
            return 1
        }
    else
        git clone --depth 1 "https://aur.archlinux.org/$aur.git" "$(pkbuild_dir "$aur")" || {
            echo "=> $0: ERROR: could not clone https://aur.archlinux.org/$aur.git" >&2
            return 1
        }
    fi
    printf '%s\n' "$(pkbuild_path "$aur")"
}

# --- already built with the current version? -----------------------------------------
# A package is up to date (no rebuild needed) when an artifact for it sits in $REPO_DIR
# and its file name contains the PKGBUILD's pkgver — i.e. the published artifact was
# produced by this exact PKGBUILD. Packages that are stale or not on disk need building.
is_current() {
    local aur="$1" pub p ver
    is_safe_name "$aur" || return 1
    [ -f "$(pkbuild_path "$aur")" ] || return 1
    pub="$(published_name "$aur")"
    # the version this PKGBUILD produces. AUR packages carry a static pkgver= or evaluate
    # their pkgver() function like makepkg does; signal-tauri-bin's is git describe, so
    # just compute it from the cloned repo — both paths must agree with the trigger.
    if [ "$aur" = "signal-tauri-bin" ]; then
        ver="$(git --git-dir "$(pkbuild_dir "$aur")/.git" rev-list --count HEAD 2>/dev/null)" || return 1
        ver="0.1.0.r${ver}.g$(git --git-dir "$(pkbuild_dir "$aur")/.git" rev-parse --short HEAD 2>/dev/null)"
    else
        # $aur is gated to a safe path component (no /, .., spaces) by is_safe_name() above,
        # so pkbuild_path resolves it under the fixed base $BUILD_DIR/src/. The $(printf %q
        # ...) assignment clears the taint chain and stops backticks/`$` from becoming
        # commands in `bash -c`. That sanitized path is the argument to `bash -c`, not $aur.
        pkg=$(printf '%q' "$(pkbuild_path "$aur")")
        ver="$(bash -c 'source "$0"; pkgver 2>/dev/null || echo ${pkgver:-0}' "$pkg" 2>/dev/null)"
    fi
    [ -z "$ver" ] && return 1
    # the artifact on disk must carry exactly this pkgver: <pub>-<pkgver>-<release>-
    p="$(ls "$REPO_DIR"/${pub}-${ver}-* 2>/dev/null | tail -1)"
    [ -z "$p" ] && p="$(ls "$REPO_DIR"/${pub%-bin}-bin-${ver}-* 2>/dev/null | tail -1)"
    [ -n "$p" ]
}

# --- find one published artifact ------------------------------------------------------
# The newest artifact for a package, across $REPO_DIR (an earlier run) and the build dirs.
# An older artifact sitting in $REPO_DIR must never shadow a newly built one: preferring
# $REPO_DIR first is exactly how stale versions survive a rebuild, so everything from
# packages.list ends up with its latest version after every run.
find_package() {
    local pub="$1" p d n tmp="$BUILD_DIR/.find_pkg.$$"
    : >"$tmp"
    for d in "$REPO_DIR" "${BUILD_DIRS[@]+"${BUILD_DIRS[@]}"}"; do
        for n in "$pub" "$(aur_name "$pub")"; do
            for p in "$d"/${n}-*.pkg.tar.zst; do
                [ -e "$p" ] || continue
                # a package with debug options builds two artifacts (main + -debug): the main
                # one is what pacman needs, so skip the -debug one. Without this it sorts ahead
                # of the main artifact and ends up in the repo instead of it.
                [[ "$(basename "$p")" == *"-debug-"* ]] && continue
                printf '%s\t%s\n' "$(basename "$p")" "$p" >>"$tmp"
            done
        done
    done
    # the newest artifact by version, from $REPO_DIR (an earlier run) or a build dir
    p="$(sort -t$'\t' -k1,1Vr "$tmp" | head -1 | cut -f2)"
    rm -f "$tmp"
    [ -z "$p" ] && return 1
    printf '%s\n' "$p"
}

# --- build dirs of the current run ----------------------------------------------------
# One dir per package under $BUILD_DIR: where makepkg in the container writes its artifact.
BUILD_DIRS=()
