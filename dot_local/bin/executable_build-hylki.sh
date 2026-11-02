#!/usr/bin/env bash
# build-hylki.sh — build hylki in an Arch container on the fleet build host
# (cyberkleiber, 32GB) and publish the resulting repo to the Raspberry Pi.
#
#   executable_build-hylki.sh [--host <cyberkleiber|lincopta>] [--pi <pi-host>]
#                             [--keep-src] [--repo-only]
#
#   --host     Build host (default: cyberkleiber). Needs docker/podman and
#              SSH access to the Pi. (currently unused — the build runs here)
#   --pi       The Pi's host name or IP (default: f3nt0t9dnh9t8l0v.myfritz.net).
#   --keep-src Keep the AUR clone in $BUILD_DIR instead of removing it.
#   --repo-only Skip the build if the package already exists; only do repo-add
#                and rsync to the Pi.
#
# Flow: clone the PKGBUILD -> set pkgname to hylki-bin -> makepkg in an Arch
# container (memory capped) -> repo-add -> rsync to the Pi -> omarchy installs
# from http://<pi>:8080/ (the repo lives in $HOME/.local/arch-repo on the Pi).
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
PKG_NAME="hylki-bin"          # the published package name
CONTAINER_ENGINE="${CONTAINER_ENGINE:-docker}"
BUILD_HOST="${BUILD_HOST:-cyberkleiber}"
PI_HOST="${PI_HOST:-f3nt0t9dnh9t8l0v.myfritz.net}"
PI_USER="jan"
PI_DIR="$HOME/.local/arch-repo"

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

# --- the build dir must be owned by us: a previous container run hands the
#     ownership of $BUILD_DIR over to a container-internal UID (podman rootless
#     maps container UIDs onto another host range), which silently kills the
#     script — sed -i and the package lookup both fail on an unreadable tree. ---
if ! [ -w "$BUILD_DIR" ]; then
    echo "=> $SCRIPT_NAME: $BUILD_DIR is not writable by $(whoami) — fixing ownership"
    sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
        echo "=> $SCRIPT_NAME: ERROR: could not fix ownership of $BUILD_DIR"
        exit 1
    }
fi

# --- get the PKGBUILD ---
if [ ! -d "$BUILD_DIR/hylki" ]; then
    echo "=> $SCRIPT_NAME: cloning the hylki PKGBUILD"
    git clone --depth 1 https://aur.archlinux.org/hylki.git "$BUILD_DIR/hylki"
fi

# --- the published package must be named $PKG_NAME ---
# The AUR hylki PKGBUILD ships pkgname=(hylki); set it to $PKG_NAME so the
# published artifact is hylki-bin-<ver>.pkg.tar.zst.
# The source tarball and prepare() still use the AUR name (hylki-<ver>), so
# they must be adjusted in parallel.
if ! grep -q "^pkgname=(\$PKG_NAME)" "$BUILD_DIR/hylki/PKGBUILD"; then
    echo "=> $SCRIPT_NAME: setting pkgname to $PKG_NAME in the PKGBUILD"
    sed -i "s/^pkgname=.*/pkgname=($PKG_NAME)/" "$BUILD_DIR/hylki/PKGBUILD"
    sed -i 's|"\$pkgname-\$pkgver.tar.gz::"|"hylki-\$pkgver.tar.gz::"|' "$BUILD_DIR/hylki/PKGBUILD"
    sed -i 's|cd "\$pkgname-\$pkgver"|cd hylki-\$pkgver|' "$BUILD_DIR/hylki/PKGBUILD"
fi

# --- build in Arch container (memory capped so the build host stays alive) ---
# makepkg refuses to run as root, so the container starts as root, creates a
# non-root 'builder' user and hands over the build to it.
# The build dir must stay owned by us: the container must not chown it to a
# container-internal UID (podman rootless maps container UIDs onto another host
# range), so we make it writable instead of chowning it, and restore the
# ownership after the build.
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
        bash -c "useradd -m -s /bin/bash builder && echo 'builder ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/builder && chmod 440 /etc/sudoers.d/builder && chmod 777 /builds && [ -d /builds/hylki ] && chmod 777 /builds/hylki && su builder -c 'cd /builds/hylki && sudo pacman -Sy --noconfirm && makepkg --syncdeps --noconfirm --nocheck'" || {
        echo "=> $SCRIPT_NAME: container build failed"
        exit 1
    }
    sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
        echo "=> $SCRIPT_NAME: ERROR: could not restore ownership of $BUILD_DIR"
        exit 1
    }
    chmod 755 "$BUILD_DIR"
else
    echo "=> $SCRIPT_NAME: package already built, skipping build (--repo-only)"
fi

# --- collect the package (makepkg lands it in the build dir, the repo is
#     a separate dir) ---
# the artifact is named $PKG_NAME-<ver>-<rel>-<arch>.pkg.tar.zst; a previous run
# of this script (before the rename to hylki-bin) left hylki-<ver>-<rel>-<arch>
# behind, so fall back to the AUR name there. `head -1` keeps this single-valued
# even when the glob matches more than one artifact. `--repo-only` finds the
# package in the repo dir (rsynced from a previous run).
find_package() {
    local p d
    # the $PKG_NAME artifact first (hylki-bin-<ver>-<rel>-<arch>), then the AUR
    # name left behind by an older run of this script (hylki-<ver>-<rel>-<arch>)
    for d in "$REPO_DIR" "$BUILD_DIR/hylki"; do
        p="$(ls "$d"/${PKG_NAME}-*.pkg.tar.zst 2>/dev/null | head -1)"
        if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
    done
    for d in "$REPO_DIR" "$BUILD_DIR/hylki"; do
        p="$(ls "$d"/hylki-*.pkg.tar.zst 2>/dev/null | head -1)"
        if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
    done
    return 1
}
if [ "$REPO_ONLY" = "1" ]; then
    # --repo-only: only the published $PKG_NAME artifact counts
    PKG="$(ls "$REPO_DIR"/${PKG_NAME}-*.pkg.tar.zst 2>/dev/null | head -1 || true)"
    if [ -z "$PKG" ]; then
        echo "=> $SCRIPT_NAME: no $PKG_NAME package in $REPO_DIR"
        exit 1
    fi
else
    PKG="$(find_package)"
    if [ -z "$PKG" ]; then
        echo "=> $SCRIPT_NAME: no package produced in $BUILD_DIR/hylki/"
        exit 1
    fi
fi
# --- clean stale artifacts out of $REPO_DIR (leftovers from earlier runs) ---
for f in "$REPO_DIR"/*.pkg.tar.zst; do
    [ "$(basename "$f")" = "$(basename "$PKG")" ] && continue
    rm -f "$f"
done
if ! [ -f "$REPO_DIR/$(basename "$PKG")" ]; then
    cp "$PKG" "$REPO_DIR/"
fi
echo "=> $SCRIPT_NAME: packages: $(basename "$PKG")"

PKG_FILE="$(basename "$PKG")"
echo "=> $SCRIPT_NAME: package: $PKG_FILE"

# --- generate repo db (repo-add is an Arch tool — run it in the container) ---
$CONTAINER_ENGINE run --rm \
    -v "$REPO_DIR:/repo" \
    archlinux:base-devel \
    bash -c "repo-add /repo/repo.db.tar.gz /repo/$PKG_FILE" 2>&1 | tail -5 || {
        echo "=> $SCRIPT_NAME: repo-add failed (unsigned package? check SIGNKEY in makepkg.conf)"
        exit 1
    }
if [ ! -f "$REPO_DIR/repo.db.tar.gz" ]; then
    echo "=> $SCRIPT_NAME: repo-add failed (unsigned package? check SIGNKEY in makepkg.conf)"
    exit 1
fi
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
