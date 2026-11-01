#!/usr/bin/env bash
# build-hylki.sh — build hylki in an Arch container on the fleet build host
# (cyberkleiber, 32GB) and publish the resulting repo to the Raspberry Pi.
#
#   executable_build-hylki.sh [--host <cyberkleiber|lincopta>] [--pi <pi-host>]
#                             [--keep-src] [--repo-only]
#
#   --host     Build host (default: cyberkleiber). Needs docker/podman and
#              SSH access to the Pi.
#   --pi       The Pi's host name or IP (default: raspberry).
#   --keep-src Keep the AUR clone in $BUILD_DIR instead of removing it.
#   --repo-only Skip the build if the package already exists; only do repo-add
#                and rsync to the Pi.
#
# Flow: clone the PKGBUILD -> makepkg in an Arch container (memory capped) ->
# repo-add -> rsync to the Pi -> omarchy installs from http://<pi>:8080/
#
# Sudo is used only for the memory-capped container (needed on the build host)
# and the run is scoped to the build host via the hostname guard below.

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"

# --- hostname guard: only meaningful on the build hosts ---
if [ "$(hostname)" != "cyberkleiber" ] && [ "$(hostname)" != "lincopta" ]; then
    echo "=> $SCRIPT_NAME: only intended for the fleet build hosts (cyberkleiber/lincopta)"
    echo "=> $SCRIPT_NAME: run on the build host, not on $(hostname)"
    exit 1
fi

BUILD_DIR="${BUILD_DIR:-/home/jan/builds}"
REPO_DIR="$BUILD_DIR/repo"
AUR_PKG="hylki"
CONTAINER_ENGINE="${CONTAINER_ENGINE:-docker}"
BUILD_HOST="${BUILD_HOST:-cyberkleiber}"
PI_HOST="${PI_HOST:-f3nt0t9dnh9t8l0v.myfritz.net}"
PI_USER="jan"
PI_DIR="/srv/repo"

KEEP_SRC=0
REPO_ONLY=0
while [ $# -gt 0 ]; do
    case "$1" in
        --host)
            BUILD_HOST="$2"; shift 2 ;;
        --host=*)
            BUILD_HOST="${1#*=}"; shift ;;
        --pi)
            PI_HOST="$2"; shift 2 ;;
        --pi=*)
            PI_HOST="${1#*=}"; shift ;;
        --keep-src)
            KEEP_SRC=1; shift ;;
        --repo-only)
            REPO_ONLY=1; shift ;;
        --repo-only=*)
            REPO_ONLY="${1#*=}"; shift ;;
        -h|--help)
            head -8 "$0"; exit 0 ;;
        *)
            echo "=> $SCRIPT_NAME: unknown arg: $1"; exit 1 ;;
    esac
done

echo "=> $SCRIPT_NAME: building on $BUILD_HOST, publishing to pi@$PI_HOST"

# --- setup ---
mkdir -p "$REPO_DIR"
mkdir -p "$BUILD_DIR/src"

# --- get the PKGBUILD ---
if [ ! -d "$BUILD_DIR/hylki" ]; then
    echo "=> $SCRIPT_NAME: cloning the hylki PKGBUILD"
    git clone --depth 1 https://aur.archlinux.org/hylki.git "$BUILD_DIR/hylki"
fi

# --- build in Arch container (memory capped so the build host stays alive) ---
# makepkg's parallelism is controlled by MAKEFLAGS (no -j option in modern
# makepkg) — -j1 keeps the build from thrashing the build host's RAM.
# makepkg refuses to run as root, and the volume mount lands root-owned in the
# container, so the container starts as root, makes the mount usable and then
# hands over to a non-root 'builder' user.
# archlinux:base-devel has makepkg + fakeroot + debugedit (the minimal
# archlinux image does not).
# --repo-only skips the build if the package already exists (repo-add + rsync
# only).
if [ "$REPO_ONLY" != "1" ]; then
    echo "=> $SCRIPT_NAME: building $AUR_PKG in $CONTAINER_ENGINE (archlinux:base-devel)"
    $CONTAINER_ENGINE run --rm \
        --memory=24g --memory-swap=24g \
        --network host \
        -v "$BUILD_DIR:/builds" \
        -e MAKEFLAGS="-j1" \
        archlinux:base-devel \
        bash -c "useradd -m -s /bin/bash builder && echo 'builder ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/builder && chmod 440 /etc/sudoers.d/builder && chown -R builder:builder /builds && su builder -c 'cd /builds/hylki && sudo pacman -Sy --noconfirm && makepkg --syncdeps --noconfirm --nocheck'"
else
    echo "=> $SCRIPT_NAME: package already built, skipping build (--repo-only)"
fi

# --- collect the package (makepkg lands it in the build dir, the repo is
#     a separate dir) ---
PKG=$(ls "$BUILD_DIR"/hylki/hylki-*.pkg.tar.zst 2>/dev/null || true)
if [ -z "$PKG" ]; then
    echo "=> $SCRIPT_NAME: no package produced in $BUILD_DIR/hylki/"
    exit 1
fi
for p in $PKG; do cp "$p" "$REPO_DIR/"; done
echo "=> $SCRIPT_NAME: packages: $(basename $PKG | tr '\n' ' ')"

PKG_FILE=$(ls "$REPO_DIR"/hylki-*.pkg.tar.zst 2>/dev/null | head -1 || true)
if [ -z "$PKG_FILE" ]; then
    echo "=> $SCRIPT_NAME: no package produced in $REPO_DIR"
    exit 1
fi
echo "=> $SCRIPT_NAME: package: $(basename "$PKG_FILE")"

# --- generate repo db (repo-add is an Arch tool — run it in the container) ---
$CONTAINER_ENGINE run --rm \
    -v "$REPO_DIR:/repo" \
    archlinux:base-devel \
    bash -c "repo-add /repo/repo.db.tar.gz /repo/$(basename "$PKG_FILE")" 2>&1 | tail -5 || {
        echo "=> $SCRIPT_NAME: repo-add failed (unsigned package? check SIGNKEY in makepkg.conf)"
        exit 1
    }
if [ ! -f "$REPO_DIR/repo.db.tar.gz" ]; then
    echo "=> $SCRIPT_NAME: repo-add failed (unsigned package? check SIGNKEY in makepkg.conf)"
    exit 1
fi
echo "=> $SCRIPT_NAME: repo.db generated"
echo "=> $SCRIPT_NAME: repo.db generated"

# --- sync to the Pi ---
echo "=> $SCRIPT_NAME: rsyncing repo to pi@$PI_HOST:$PI_DIR"
# the Pi's SSH listener is on 6622 (myfritz), not the default 22
rsync -a --delete -e "ssh -p 6622" "$REPO_DIR/" "$PI_USER@$PI_HOST:$PI_DIR/" || {
    echo "=> $SCRIPT_NAME: rsync to the Pi failed"
    exit 1
}
echo "=> $SCRIPT_NAME: repo synced to pi@$PI_HOST:$PI_DIR"

# --- housekeeping ---
if [ "$KEEP_SRC" == "0" ]; then
    rm -rf "$BUILD_DIR/hylki"
fi

echo "=> $SCRIPT_NAME: done. omarchy: pacman -Sy && pacman -S hylki (from http://<pi>:8080/)"
