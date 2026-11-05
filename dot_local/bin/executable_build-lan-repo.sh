#!/usr/bin/env bash
# executable_build-lan-repo.sh — build AUR packages in an Arch container on the
# fleet build host (cyberkleiber, 32GB) and publish them to the lan_repo on the
# Raspberry Pi.
#
#   executable_build-lan-repo.sh [packages.list] [pkg1 pkg2...] [--repo-only]
#                                [--keep-src] [--reconcile]
#
#   [packages.list]    Policy file (default: $HOME/.local/build-lan-reo/packages.list,
#                      the chezmoi-rendered one): everything on it must be served
#                      by the lan-repo, nothing else. Every call reconciles the repo
#                      against it — packages that leave the list are dropped.
#   [pkg1 pkg2...]     AUR names to build (overrides the policy list).
#   --package <name>   same as giving the name (backwards compatible flag).
#   --reconcile        skip all builds: drop everything not on packages.list and
#                      regenerate the db (the rebuild trigger uses this when nothing
#                      moved so a package removed from packages.list really vanishes).
#   --repo-only        skip the container build; only repo-add + rsync.
#   --keep-src         keep the AUR clones in $BUILD_DIR instead of removing them.
#
# Flow: clone PKGBUILDs -> makepkg in one memory-capped Arch container
# (archlinux:base-devel) -> repo-add (only what remains on disk after reconciliation)
# -> rsync to the Pi -> mark last-built versions. The lan-repo serves exactly
# packages.list + latest versions after every run.
#
# Build performance on the workstation host: makepkg uses MAKEFLAGS (parallel),
# cargo/rustc read CARGO_BUILD_JOBS (crates in parallel) and RUSTFLAGS
# (-C target-cpu=native picks Skylake SP, -C parallel-compiler splits one crate's
# codegen across threads; rustc >= 1.78). These env vars are inert for non-rust
# package builds (gcc/clang never read them), so they can sit on the container
# run line alongside the shared flags.
#
# Sudo is used only for the memory-capped container (needed on the build host)
# and the run is scoped to the build host via the hostname guard below.

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"

log() { echo "=> $SCRIPT_NAME: $*" >&2; }

# --- hostname guard: only meaningful on the build hosts -----------------------
if [ "$(hostname)" != "cyberkleiber" ] && [ "$(hostname)" != "lincopta" ]; then
    log "only intended for the fleet build hosts (cyberkleiber/lincopta); run on the build host, not on $(hostname)"
    exit 1
fi

BUILD_DIR="${BUILD_DIR:-/home/jan/builds}"
REPO_DIR="$BUILD_DIR/repo"
CONTAINER_ENGINE="${CONTAINER_ENGINE:-docker}"
BUILD_HOST="${BUILD_HOST:-cyberkleiber}"
PI_HOST="${PI_HOST:-f3nt0t9dnh9t8l0v.myfritz.net}"
PI_USER="jan"
PI_DIR="$HOME/.local/arch-repo"
MARKER_DIR="$BUILD_DIR/.last-built"
NPROC=$(nproc 2>/dev/null || echo 1)

KEEP_SRC=0
REPO_ONLY=0
RECONCILE_ONLY=0
PACKAGES=()
POSITIONAL=()
while [ $# -gt 0 ]; do
    case "$1" in
        --host)        BUILD_HOST="$2"; shift 2 ;;
        --host=*)      BUILD_HOST="${1#*=}"; shift 2 ;;
        --pi)          PI_HOST="$2"; shift 2 ;;
        --pi=*)        PI_HOST="${1#*=}"; shift 2 ;;
        --keep-src)    KEEP_SRC=1; shift ;;
        --repo-only)   REPO_ONLY=1; shift ;;
        --reconcile)   RECONCILE_ONLY=1; shift ;;
        --package)     PACKAGES+=("$2"); shift 2 ;;
        --package=*)   PACKAGES+=("${1#*=}"); shift ;;
        -h|--help)     head -8 "$0"; exit 0 ;;
        *)             POSITIONAL+=("$1"); shift ;;
    esac
done

# --- packages.list: the policy (the authority for what the repo serves) ---------
LIST="${LAN_REPO_LIST:-$HOME/.local/build-lan-repo/packages.list}"
PACKAGES_FROM_LIST=()
if [ -f "$LIST" ]; then
    while IFS= read -r line || [ -n "$line" ]; do
        line="${line// /}"                # trim whitespace
        [[ -z "$line" || "$line" == \#* ]] && continue   # blank / comment
        PACKAGES_FROM_LIST+=("$line")
    done < "$LIST"
fi

# --- the package set: explicit args win, then the policy list -----------------
if [ ${#POSITIONAL[@]} -gt 0 ] || [ ${#PACKAGES[@]} -gt 0 ]; then
    BUILD_SET=("${PACKAGES[@]+"${PACKAGES[@]}"}" "${POSITIONAL[@]}")
else
    if [ ! -f "$LIST" ]; then
        log "ERROR: no packages to build (no arguments and $LIST not found)"
        exit 1
    fi
    BUILD_SET=("${PACKAGES_FROM_LIST[@]}")
fi

echo "=> $SCRIPT_NAME: building ${#BUILD_SET[@]} package(s) on $BUILD_HOST, publishing to pi@$PI_HOST"
for p in "${BUILD_SET[@]}"; do echo "   -> $p"; done

# --- the published name of one AUR package --------------------------------------
published_name() {
    local aur="$1"
    if [[ "$aur" == *"-bin" ]]; then
        printf '%s\n' "$aur"
    else
        printf '%s\n' "${aur}-bin"
    fi
}

# --- the AUR name of a published artifact (fallback for leftovers) --------------
# hylki-bin -> hylki (the AUR package), hylki -> hylki.
aur_name() {
    local pub="$1"
    if [[ "$pub" == *"-bin" ]]; then
        printf '%s\n' "${pub%-bin}"
    else
        printf '%s\n' "$pub"
    fi
}

# --- the build dir and PKGBUILD of one package ----------------------------------
pkbuild_dir() { printf '%s\n' "$BUILD_DIR/$1"; }
pkbuild_path() { printf '%s\n' "$BUILD_DIR/$1/PKGBUILD"; }

# --- get the PKGBUILD for one package -------------------------------------------
# (the dir can exist and be empty from a previous failed run, so verify it)
get_pkbuild() {
    local aur="$1"
    if [ ! -f "$(pkbuild_path "$aur")" ]; then
        echo "=> $SCRIPT_NAME: cloning the $aur PKGBUILD" >&2
        sudo rm -rf "$(pkbuild_dir "$aur")"
        git clone --depth 1 "https://aur.archlinux.org/$aur.git" "$(pkbuild_dir "$aur")" || {
            echo "=> $SCRIPT_NAME: ERROR: could not clone https://aur.archlinux.org/$aur.git" >&2
            return 1
        }
    fi
    printf '%s\n' "$(pkbuild_path "$aur")"
}

# --- already built with the current version? -------------------------------------
# A package is up to date (no rebuild needed) when an artifact for it sits in $REPO_DIR
# and its file name contains the PKGBUILD's pkgver — i.e. the published artifact was
# produced by this exact PKGBUILD. Packages that are stale or not on disk need building.
is_current() {
    local aur="$1" pub p ver
    [ -f "$(pkbuild_path "$aur")" ] || return 1
    pub="$(published_name "$aur")"
    ver="$(grep -m1 '^pkgver=' "$(pkbuild_path "$aur")" 2>/dev/null | cut -d= -f2 || true)"
    [ -z "$ver" ] && return 1
    # the artifact on disk must carry exactly this pkgver: <pub>-<pkgver>-<release>-
    p="$(ls "$REPO_DIR"/${pub}-${ver}-* 2>/dev/null | tail -1)"
    [ -n "$p" ]
}

# --- setup -----------------------------------------------------------------------
mkdir -p "$REPO_DIR"
mkdir -p "$BUILD_DIR/src"

# --- the build dir must be owned by us: a previous container run hands the
#     ownership of $BUILD_DIR over to a container-internal UID (podman rootless
#     maps container UIDs onto another host range), which silently kills the
#     script — sed -i and the package lookup both fail on an unreadable tree. ---
if ! [ -w "$BUILD_DIR" ]; then
    log "$BUILD_DIR is not writable by $(whoami) — fixing ownership"
    sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
        log "ERROR: could not fix ownership of $BUILD_DIR"
        exit 1
    }
fi

# --- clone every PKGBUILD (must exist before the container run) -----------------
# and record where each one lives so its artifact can be found afterwards.
BUILD_DIRS=()
for aur in "${BUILD_SET[@]}"; do
    if ! get_pkbuild "$aur" >/dev/null; then
        log "ERROR: could not get the PKGBUILD for $aur"
        exit 1
    fi
    BUILD_DIRS+=("$(pkbuild_dir "$aur")/pkg")
done

# --- omit already-built packages (no rebuild needed) ------------------------------
# A package that was published with this exact PKGBUILD is up to date: its artifact
# for pkgver=<pkgver> sits in $REPO_DIR, so it stays on disk and in the db while
# nothing is rebuilt in the container. Only when a package moves (stale or missing
# artifact) does it get built.
ORIG_SET=("${BUILD_SET[@]+"${BUILD_SET[@]}"}")
REBUILD_SET=()
SKIPPED=()
for aur in "${BUILD_SET[@]}"; do
    if is_current "$aur" 2>/dev/null; then
        artifact="$(ls "$REPO_DIR"/$(published_name "$aur")-* 2>/dev/null | tail -1)"
        log "omitting $aur (already built: $(basename "$artifact"))"
        SKIPPED+=("$aur")
    else
        REBUILD_SET+=("$aur")
    fi
done
if [ ${#REBUILD_SET[@]} -eq 0 ]; then
    log "no package needs rebuilding (all up to date) — only reconciling the repo"
fi

# --- build all packages in one container -----------------------------------------
if [ "$RECONCILE_ONLY" != "1" ] && [ "$REPO_ONLY" != "1" ] && [ ${#REBUILD_SET[@]} -gt 0 ]; then
    echo "=> $SCRIPT_NAME: building ${REBUILD_SET[*]} in $CONTAINER_ENGINE (archlinux:base-devel)"
    echo "   (skipped ${#SKIPPED[@]} already-built of the original ${#ORIG_SET[@]})"
    # Each package is built in its own work dir, so write one build script for the
    # container (avoids all quoting layers of `bash -c` inside `bash -c`). The
    # build dirs must be writable for the container's non-root builder user, because
    # podman rootless maps a container UIDs onto another host range.
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
        -e MAKEFLAGS="-j$NPROC" \
        -e CARGO_BUILD_JOBS="$NPROC" \
        -e RUSTFLAGS="-C target-cpu=native -C parallel-compiler" \
        archlinux:base-devel \
        bash /builds/.build.sh "${REBUILD_SET[@]}" || {
        echo "=> $SCRIPT_NAME: container build failed"
        exit 1
    }
    sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
        log "ERROR: could not restore ownership of $BUILD_DIR"
        exit 1
    }
    chmod 755 "$BUILD_DIR"
else
    echo "=> $SCRIPT_NAME: skipping the container build (--repo-only/--reconcile)"
fi

# --- find one published artifact ----------------------------------------------------
# $REPO_DIR first (an earlier run), then each package's build dir.
find_package() {
    local pub="$1"
    local p d n
    for d in "$REPO_DIR" "${BUILD_DIRS[@]+"${BUILD_DIRS[@]}"}"; do
        for n in "$pub" "$(aur_name "$pub")"; do
            p="$(ls "$d"/${n}-*.pkg.tar.zst 2>/dev/null | tail -1)"
            if [ -n "$p" ]; then printf '%s\n' "$p"; return 0; fi
        done
    done
    return 1
}

# --- publish all packages ---------------------------------------------------------
# The published artifact is <published-name>-<version>-<arch>.pkg.tar.zst, e.g.
# hylki-bin-1.47.0-1-x86_64.pkg.tar.zst.
PUBLISHED=()
for aur in "${BUILD_SET[@]}"; do
    pub="$(published_name "$aur")"
    # the newest artifact: from $REPO_DIR (an earlier run) or the package's build dir
    PKG="$(find_package "$pub" || true)"
    if [ -z "$PKG" ]; then
        log "ERROR: no $pub package found for $aur (build dir or repo)"
        exit 1
    fi
    cp "$PKG" "$REPO_DIR/"
    PUBLISHED+=("$aur")
    echo "=> $SCRIPT_NAME: $aur -> $pub ($(basename "$PKG"))"
done

# --- reconcile: what remains on disk after dropping everything not in packages.list -
# Every package that leaves packages.list is dropped (plus its db entry), so the
# lan-repo serves exactly packages.list + latest versions after every run.
if [ -f "$LIST" ]; then
    REMAINING=()
    for f in "$REPO_DIR"/*.pkg.tar.zst; do
        [ -f "$f" ] || continue
        name="$(basename "$f")"
        kept=0
        for pkg in "${PACKAGES_FROM_LIST[@]}"; do
            [[ "$name" == "$(published_name "$pkg")-"* ]] && kept=1
        done
        if [ $kept -eq 1 ]; then
            REMAINING+=("$f")
        else
            rm -f "$f"
            log "dropped stale artifact $(basename "$f") (not in $LIST)"
        fi
    done
else
    # no policy file — keep everything on disk; the db is regenerated below from
    # all artifacts that are there.
    for f in "$REPO_DIR"/*.pkg.tar.zst; do
        [ -f "$f" ] || continue
        REMAINING+=("$f")
    done
fi

# --- regenerate the db (repo-add is an Arch tool — run it in the container) --------
# The db must contain ONLY what is on disk: a stale db (entries for packages that
# are not on disk) makes pacman fail when it tries to download missing files.
rm -f "$REPO_DIR/repo.db.tar.gz" "$REPO_DIR/repo.db.tar.gz.old" \
      "$REPO_DIR/repo.files.tar.gz" "$REPO_DIR/repo.files.tar.gz.old"
ARGS=()
for f in "${REMAINING[@]}"; do ARGS+=("/repo/$(basename "$f")"); done
if [ ${#ARGS[@]} -gt 0 ]; then
    $CONTAINER_ENGINE run --rm \
        -v "$REPO_DIR:/repo" \
        archlinux:base-devel \
        bash -c 'repo-add /repo/repo.db.tar.gz "$@"' bash "${ARGS[@]}" 2>&1 | tail -5 || {
        echo "=> $SCRIPT_NAME: repo-add failed (unsigned package? check SIGNKEY in makepkg.conf)"
        exit 1
    }
fi
if [ ! -f "$REPO_DIR/repo.db.tar.gz" ]; then
    log "ERROR: repo-add did not produce a db (no artifacts on disk? see logs)"
    exit 1
fi
log "repo.db regenerated ($(ls "$REPO_DIR"/*.pkg.tar.zst | wc -l) artifact(s) on disk)"

# --- sync to the Pi ---------------------------------------------------------------
echo "=> $SCRIPT_NAME: rsyncing repo to pi@$PI_HOST:$PI_DIR"
# the Pi's SSH listener is on 6622 (myfritz), not the default 22
rsync -a --delete -e "ssh -p 6622" "$REPO_DIR/" "$PI_USER@$PI_HOST:$PI_DIR/" || {
    log "ERROR: rsync to the Pi failed"
    exit 1
}
echo "=> $SCRIPT_NAME: repo synced to pi@$PI_HOST:$PI_DIR"

# --- housekeeping -----------------------------------------------------------------
# mark what was last built ($BUILD_DIR/<pkg>/.last-built-pkgver, with a stash copy
# that survives the package dir cleanup below) and remove the AUR clones.
if [ ${#PUBLISHED[@]} -gt 0 ]; then
    mkdir -p "$MARKER_DIR"
    for aur in "${PUBLISHED[@]}"; do
        ver="$(grep -m1 '^pkgver=' "$(pkbuild_path "$aur")" 2>/dev/null | cut -d= -f2 || true)"
        if [ -z "$ver" ]; then
            # fall back to the version baked into the artifact's name on disk
            pat="$REPO_DIR/$(published_name "$aur")-*"
            ver="$(ls $pat 2>/dev/null | sed 's|.*/||' | grep -m1 -oE '[0-9]+(\.[0-9]+)+(-[a-zA-Z0-9]+)?')"
        fi
        if [ -z "$ver" ]; then
            log "WARNING: could not determine pkgver for $aur (marker untouched)"
            continue
        fi
        mkdir -p "$(pkbuild_dir "$aur")" 2>/dev/null || true
        echo "$ver" > "$(pkbuild_path "$aur")/.last-built-pkgver"
        cp "$(pkbuild_path "$aur")/.last-built-pkgver" "$MARKER_DIR/$aur" 2>/dev/null || true
        log "marked $aur built: $ver ($MARKER_DIR/$aur)"
    done
fi

if [ "$KEEP_SRC" == "0" ]; then
    for aur in "${BUILD_SET[@]}"; do
        # preserve the marker before the package dir is removed (it survives here)
        [ -f "$(pkbuild_dir "$aur")/.last-built-pkgver" ] && \
            cp "$(pkbuild_dir "$aur")/.last-built-pkgver" "$MARKER_DIR/$aur" 2>/dev/null || true
        sudo rm -rf "$(pkbuild_dir "$aur")"
    done
fi

echo "=> $SCRIPT_NAME: done. omarchy: pacman -Sy && pacman -S <package> (from http://<pi>:8080/)"
