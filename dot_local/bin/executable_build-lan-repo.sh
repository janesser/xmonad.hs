#!/usr/bin/env bash
# executable_build-lan-repo.sh — build AUR packages in an Arch container on the
# fleet build host (cyberkleiber, 32GB) and publish them to the lan_repo on the
# Raspberry Pi.
#
#   executable_build-lan-repo.sh [--host <cyberkleiber|lincopta>]
#                                [--pi <pi-host>] [--keep-src] [--repo-only]
#                                [--package <aur-name>...]
#
#   --package    AUR package to build; may be given multiple times. Default:
#                hylki (the published name is <aur-name>-bin, unless it already
#                ends in -bin).
#   --host       Build host (default: cyberkleiber). Needs docker/podman and
#                SSH access to the Pi.
#   --pi         The Pi's host name or IP (default: f3nt0t9dnh9t8l0v.myfritz.net).
#   --keep-src   Keep the AUR clones in $BUILD_DIR instead of removing them.
#   --repo-only  Skip the build if the packages already exist; only do repo-add
#                and rsync to the Pi.
#
# Flow: clone each PKGBUILD -> makepkg in an Arch container (memory capped) ->
# repo-add -> rsync to the Pi -> omarchy installs from http://<pi>:8080/
# (the repo lives in $HOME/.local/arch-repo on the Pi).
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
CONTAINER_ENGINE="${CONTAINER_ENGINE:-docker}"
BUILD_HOST="${BUILD_HOST:-cyberkleiber}"
PI_HOST="${PI_HOST:-f3nt0t9dnh9t8l0v.myfritz.net}"
PI_USER="jan"
PI_DIR="$HOME/.local/arch-repo"

KEEP_SRC=0
REPO_ONLY=0
PACKAGES=()
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
        --package)
            PACKAGES+=("$2"); shift 2 ;;
        --package=*)
            PACKAGES+=("${1#*=}"); shift ;;
        -h|--help)
            head -8 "$0"; exit 0 ;;
        *)
            echo "=> $SCRIPT_NAME: unknown arg: $1"; exit 1 ;;
    esac
done

# default: the package everything was originally built for
if [ ${#PACKAGES[@]} -eq 0 ]; then
    PACKAGES=(hylki)
fi

echo "=> $SCRIPT_NAME: building ${#PACKAGES[@]} package(s) on $BUILD_HOST, publishing to pi@$PI_HOST"
for p in "${PACKAGES[@]}"; do echo "   -> $p"; done

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

# --- the published package name: <aur-name>-bin, unless it already is ---
published_name() {
    local aur="$1"
    if [[ "$aur" == *"-bin" ]]; then
        printf '%s\n' "$aur"
    else
        printf '%s\n' "${aur}-bin"
    fi
}

# --- the AUR name of a published artifact (fallback for leftovers) ------------
aur_name() {
    local pub="$1"
    if [[ "$pub" == *"-bin" ]]; then
        printf '%s\n' "${pub%-bin}"
    else
        printf '%s\n' "$pub"
    fi
}

# --- get the PKGBUILD for one package -----------------------------------------
# (the dir can exist and be empty from a previous failed run, so verify it)
get_pkbuild() {
    local aur="$1"
    if [ ! -f "$BUILD_DIR/$aur/PKGBUILD" ]; then
        echo "=> $SCRIPT_NAME: cloning the $aur PKGBUILD"
        sudo rm -rf "$BUILD_DIR/$aur"
        git clone --depth 1 "https://aur.archlinux.org/$aur.git" "$BUILD_DIR/$aur" || {
            echo "=> $SCRIPT_NAME: ERROR: could not clone https://aur.archlinux.org/$aur.git"
            return 1
        }
    fi
    printf '%s\n' "$BUILD_DIR/$aur/PKGBUILD"
}

# --- clone every PKGBUILD (must exist before the container run) ---------------
# The build dirs must be writable for the container's non-root builder user,
# because podman rootless maps a container UIDs onto another host range, so
# what we own here is not what the builder owns there. The container chmods
# every build dir itself before it builds.
for aur in "${PACKAGES[@]}"; do
    if ! get_pkbuild "$aur" >/dev/null; then
        exit 1
    fi
done

# --- build all packages in one container ---------------------------------------
if [ "$REPO_ONLY" != "1" ]; then
    echo "=> $SCRIPT_NAME: building ${#PACKAGES[@]} package(s) in $CONTAINER_ENGINE (archlinux:base-devel)"
    # Each package is built in its own work dir. The build dirs are chmod'ed 777
    # for the container's non-root builder user; ownership is restored after.
    echo "=> $SCRIPT_NAME: building ${PACKAGES[*]} in one container"
    # Each package is built in its own work dir, so write one build script for
    # the container (avoids all quoting layers of `bash -c` inside `bash -c`).
    cat > "$BUILD_DIR/.build.sh" <<'EOF'
useradd -m -s /bin/bash builder
printf 'builder ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/builder
chmod 440 /etc/sudoers.d/builder
chmod 777 /builds
for pkg in "$@"; do
    chmod 777 "/builds/$pkg"
    su builder -c "cd /builds/$pkg && sudo pacman -Sy --noconfirm && makepkg --syncdeps --noconfirm --nocheck"
done
EOF
    $CONTAINER_ENGINE run --rm \
        --memory=24g --memory-swap=24g \
        --network host \
        -v "$BUILD_DIR:/builds" \
        -e MAKEFLAGS="-j1" \
        archlinux:base-devel \
        bash /builds/.build.sh _ "${PACKAGES[@]}" || {
        echo "=> $SCRIPT_NAME: container build failed"
        exit 1
    }
    sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
        echo "=> $SCRIPT_NAME: ERROR: could not restore ownership of $BUILD_DIR"
        exit 1
    }
    chmod 755 "$BUILD_DIR"
else
    echo "=> $SCRIPT_NAME: packages already built, skipping build (--repo-only)"
fi

# --- find one published artifact (published name first, AUR name as fallback) --
find_package() {
    local pub="$1"
    local p d
    for d in "$REPO_DIR" "$BUILD_DIR/hylki"; do
        p="$(ls "$d"/${pub}-*.pkg.tar.zst 2>/dev/null | head -1)"
        if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
    done
    for d in "$REPO_DIR" "$BUILD_DIR/hylki"; do
        p="$(ls "$d"/$(aur_name "$pub")-*.pkg.tar.zst 2>/dev/null | head -1)"
        if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
    done
    for aur in "${PACKAGES[@]}"; do
        for d in "$REPO_DIR" "$BUILD_DIR/$aur"; do
            p="$(ls "$d"/${pub}-*.pkg.tar.zst 2>/dev/null | head -1)"
            if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
            p="$(ls "$d"/$(aur_name "$pub")-*.pkg.tar.zst 2>/dev/null | head -1)"
            if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
        done
    done
    return 1
}

# --- clean stale artifacts out of $REPO_DIR (leftovers from earlier runs) ---
for f in "$REPO_DIR"/*.pkg.tar.zst; do
    [ -f "$f" ] || continue
    rm -f "$f"
done
echo "=> $SCRIPT_NAME: cleaned stale artifacts out of $REPO_DIR"

# --- publish all packages ------------------------------------------------------
PKG_FILES=()
for aur in "${PACKAGES[@]}"; do
    pub="$(published_name "$aur")"
    PKG="$(find_package "$pub" || true)"
    if [ -z "$PKG" ]; then
        echo "=> $SCRIPT_NAME: no $pub package found for $aur (build dir or repo)"
        exit 1
    fi
    cp "$PKG" "$REPO_DIR/"
    PKG_FILES+=("$(basename "$PKG")")
    echo "=> $SCRIPT_NAME: $aur -> $pub ($(basename "$PKG"))"
done

# --- generate repo db (repo-add is an Arch tool — run it in the container) ---
ARGS=()
for f in "${PKG_FILES[@]}"; do ARGS+=("/repo/$f"); done
$CONTAINER_ENGINE run --rm \
    -v "$REPO_DIR:/repo" \
    archlinux:base-devel \
    bash -c 'repo-add /repo/repo.db.tar.gz "$@"' bash "${ARGS[@]}" 2>&1 | tail -5 || {
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
# Files created by the container are owned by 100999 on the host (podman
# rootless maps its UIDs onto another host range), so this needs root.
if [ "$KEEP_SRC" == "0" ]; then
    for aur in "${PACKAGES[@]}"; do
        sudo rm -rf "$BUILD_DIR/$aur"
    done
fi

echo "=> $SCRIPT_NAME: done. omarchy: pacman -Sy && pacman -S <package> (from http://<pi>:8080/)"
