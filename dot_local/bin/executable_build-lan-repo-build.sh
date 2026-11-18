#!/usr/bin/env bash
# executable_build-lan-repo-build.sh — clone, patch and build AUR packages in a memory-capped
# Arch container on the fleet build host (cyberkleiber/lincopta).
#
#   executable_build-lan-repo-build.sh <flags>
#
# Flags are the same as for the family entry point: --build <pkg>, --keep-src, --repo-only
# and --reconcile. The package set, mode and flags also come from the config file written by
# build-lan-repo.sh ($BUILD_DIR/.build-lan-repo-config) so this stage can run standalone once
# that file exists (a rebuild of a dropped or partially built package without re-running the
# publish stage). Explicit flags override the config.
#
# On exit: $BUILD_DIR/<pkg> holds the PKGBUILD + build state for every package it processed;
# makepkg artifacts land next to each PKGBUILD inside the same dir.

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
source "$SCRIPT_DIR/build-lan-repo-common.sh"

set -uo pipefail

CONFIG="$BUILD_DIR/.build-lan-repo-config"

# --- read the entry's config (mode, flags, package set) ----------------------------------
MODE="full"
REPO_ONLY=0
RECONCILE_ONLY=0
KEEP_SRC=0
BUILD_SET=()
if [ -f "$CONFIG" ]; then
    while IFS='=' read -r key value || [ -n "$key" ]; do
        case "$key" in
            MODE)              MODE="$value" ;;
            KEEP_SRC)          KEEP_SRC="$value" ;;
            REPO_ONLY)         REPO_ONLY="$value" ;;
            RECONCILE_ONLY)    RECONCILE_ONLY="$value" ;;
            *)                 [ -n "$key" ] && BUILD_SET+=("$key") ;;
        esac
    done < <(sed 's/^\(.*\)=\(.*\)$/\1=\2/' "$CONFIG" 2>/dev/null)
fi

# --- parse flags (same family format as the entry point, overrides the config) -----------
while [ $# -gt 0 ]; do
    case "$1" in
        --build)     [[ $2 =~ ^[a-zA-Z0-9._-]+$ ]] || { log "ERROR: invalid package name: $2"; exit 1; }
                    BUILD_SET+=("$2"); shift 2 ;;
        --package)   [[ $2 =~ ^[a-zA-Z0-9._-]+$ ]] || { log "ERROR: invalid package name: $2"; exit 1; }
                    BUILD_SET+=("$2"); shift 2 ;;   # backwards compatible --build
        --keep-src)  KEEP_SRC=1; shift ;;
        --repo-only) REPO_ONLY=1; shift ;;
        --reconcile) RECONCILE_ONLY=1; shift ;;
        -h|--help)   head -8 "$0"; exit 0 ;;
        *)           log "WARNING: unknown flag $1 (ignored)"; shift ;;
    esac
done

[ ${#BUILD_SET[@]} -gt 0 ] || { log "ERROR: no packages to build (no --build, no config)"; exit 1; }

echo "=> $0: build stage ($MODE, ${#BUILD_SET[@]} package(s))"
for p in "${BUILD_SET[@]}"; do echo "   -> $p"; done

# --- the build dir must be owned by us: a previous container run hands the ownership of
#     $BUILD_DIR over to a container-internal UID (podman rootless maps UIDs onto another
#     host range), which silently kills the script — sed -i and the package lookup both fail
#     on an unreadable tree.
mkdir -p "$BUILD_DIR/src"

if ! [ -w "$BUILD_DIR" ]; then
    log "$BUILD_DIR is not writable by $(whoami) — fixing ownership"
    sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
        log "ERROR: could not fix ownership of $BUILD_DIR"; exit 1;
    }
fi

# --- restore jan's ownership over the whole build dir -------------------------------------
# A previous run leaves files under $BUILD_DIR owned by the container's non-root builder
# user, which maps onto a host UID that is not jan — jan cannot remove or chown them.
# Inside this rootless container, the container's root maps onto jan on the host, so
# chmod + chown everything to 0:0 there makes jan the owner of every file again. The mount
# point needs to be activated first (idmapped mount quirk).
podman run --rm \
    -v "$BUILD_DIR:/builds" \
    archlinux:base-devel \
    bash -c 'mkdir -p /builds && chmod -R u+w /builds && chown -R 0:0 /builds' || {
        log "WARNING: could not restore ownership of $BUILD_DIR (some files may be stale)"
    }

# --- clone every PKGBUILD (must exist before the container run) ---------------------------
# and record where each one lives so its artifact can be found afterwards.
# --reconcile only drops stale artifacts: it needs no PKGBUILDs at all, so skip the AUR
# clones (everything it can drop is already on disk in $REPO_DIR).
if [ "$RECONCILE_ONLY" != "1" ]; then
    for aur in "${BUILD_SET[@]}"; do
        if ! get_pkbuild "$aur" >/dev/null; then
            log "ERROR: could not get the PKGBUILD for $aur"
            exit 1
        fi
        # makepkg writes the artifact into the build dir root (next to PKGBUILD, e.g.
        # vscodium-bin-1.143.17232-1-x86_64.pkg.tar.zst); $BUILD_DIR/<pkg>/pkg is only
        # the fakeroot install tree. Search the build dir for artifacts.
        BUILD_DIRS+=("$(pkbuild_dir "$aur")")
    done
else
    echo "=> $0: --reconcile — skipping AUR clones" >&2
fi

# --- post-clone patches --------------------------------------------------------------------
# AUR PKGBUILDs that fail with the current versions of their dependencies (e.g. gitahead
# vendors libssh2 with an old CMakeLists.txt — `cmake_minimum_required(VERSION < 3.5)` is
# rejected by CMake 4) need a small patch so they build instead of being dropped from the
# repo. Apply it here, after cloning.
for aur in "${BUILD_SET[@]}"; do
    case "$aur" in
        gitahead)
            p="$(pkbuild_dir "$aur")/PKGBUILD"
            [ -f "$p" ] || continue
            # add the CMake 4 compat flag to the cmake invocation in build()
            awk '/^\tcmake \\$/ {printf "%s\n\t\t-DCMAKE_POLICY_VERSION_MINIMUM=3.5 \\\n", $0; next} {print}' "$p" > "$(dirname "$p")/PKGBUILD.tmp" && mv "$(dirname "$p")/PKGBUILD.tmp" "$p"
            # vendored cmark sets `cmake_policy(SET CMP0048 OLD)` — unsupported by CMake 4.x
            for f in $(find "$BUILD_DIR/src/$aur" "$p" -name "CMakeLists.txt" 2>/dev/null); do
                sed -i 's/cmake_policy(SET CMP0048 OLD)/cmake_policy(SET CMP0048 NEW)/' "$f" 2>/dev/null || true
            done
            log "patched PKGBUILD for $aur: cmake 4 compat (-DCMAKE_POLICY_VERSION_MINIMUM=3.5, cmark policy)"
            ;;
        signal-tauri-bin)
            # signal-tauri-bin is not on the AUR (its PKGBUILD lives in its own repo), and
            # it has no integrity entry for its git source -- makepkg fails with
            # "Integrity checks are missing for: source" without one.
            p="$(pkbuild_dir "$aur")/PKGBUILD"
            [ -f "$p" ] || continue
            printf '%s\n' 'sha256sums=("SKIP")' >> "$p"
            # its `spqr` dependency (presage's ratchet) is built with prost-build, which needs
            # `protoc` at build time -- the PKGBUILD declares only rust/cargo/git as makedepends,
            # so install protobuf here or the build fails with "Could not find protoc".
            # Its audio stack also depends on `alsa-sys`, which needs libasound/alsa.pc (alsa-lib).
            sed -i 's/^makedepends=(rust cargo git)$/makedepends=(rust cargo git protobuf alsa-lib)/' "$p"
            log "patched PKGBUILD for $aur: added sha256sums=('SKIP') (git source) + makedepends=protobuf alsa-lib (protoc needed by spqr/prost-build; alsa needed by alsa-sys)"
            # signal-tauri-bin's own Cargo.toml pins `rusqlite` to a whisperfish fork of rusqlite
            # plus `[patch.crates-io]` entries for `rusqlite` and `libsqlite3-sys`. That forces
            # SQLCipher's custom crypto provider instead of the vendored one, which is what makes
            # the linked sqlite3 library miss `sqlite3_value_dup` at load time. Move back to the
            # upstream release: crates.io `rusqlite = "0.36"` needs libsqlite3-sys ^0.34.0, and
            # presage-store-sqlite needs ^0.36.0, so cargo resolves libsqlite3-sys 0.36.0 from
            # crates.io — the upstream crate with vendored SQLCipher sources. `bundled-sqlcipher`
            # is requested by the `rusqlite` dependency and enabled by presage-store-sqlite's
            # default features, so it builds the vendored SQLCipher against the system OpenSSL.
            c="$(pkbuild_dir "$aur")/Cargo.toml"
            [ -f "$c" ] || continue
            sed -i 's|^rusqlite = { git = "https://github.com/whisperfish/rusqlite", rev = "[0-9a-f]*", features = \["bundled-sqlcipher"\] }$|rusqlite = { version = "0.36", features = ["bundled-sqlcipher"] }|' "$c"
            # drop the `[patch.crates-io]` entries for `rusqlite` and `libsqlite3-sys`
            sed -i '/whisperfish\/rusqlite/d' "$c"
            grep -q '^rusqlite = { version = "0.36"' "$c" || { log "FAILED: could not move signal-tauri-bin's rusqlite dependency back to upstream crates.io"; exit 1; }
            log "patched Cargo.toml for $aur: moved rusqlite/libsqlite3-sys back to upstream crates.io (vendored SQLCipher + bundled-sqlcipher)"
            ;;
    esac
done

# --- omit already-built packages (no rebuild needed) ----------------------------------------
# A package that was published with this exact PKGBUILD is up to date: its artifact for
# pkgver=<pkgver> sits in $REPO_DIR, so it stays on disk and in the db while nothing is
# rebuilt in the container. Only when a package moves (stale or missing artifact) does it
# get built.
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

# --- build all packages in one container ---------------------------------------------------
# The container's non-root builder user needs ownership of each package tree: files created
# by root in this container map onto jan on the host, and the builder has no CAP_FOWNER, so
# makepkg's own chmod of $srcdir fails with "Operation not permitted". Own the tree with the
# builder before makepkg runs, then everything that needs touching is its own.
if [ "$RECONCILE_ONLY" != "1" ] && [ "$REPO_ONLY" != "1" ] && [ ${#REBUILD_SET[@]} -gt 0 ]; then
    echo "=> $0: building ${REBUILD_SET[*]} in a memory-capped archlinux:base-devel container"
    echo "   (skipped ${#SKIPPED[@]} already-built of the original ${#ORIG_SET[@]})"
    # Each package is built in its own work dir, so write one build script for the container
    # (avoids all quoting layers of `bash -c` inside `bash -c`).
    cat > "$BUILD_DIR/.build.sh" <<'EOF'
useradd -m -s /bin/bash builder
printf 'builder ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/builder
chmod 440 /etc/sudoers.d/builder
chmod 777 /builds
for pkg in "$@"; do
    chmod 777 "/builds/$pkg"
    # files created by root in this container are owned by jan on the host, and the
    # non-root builder user here has no CAP_FOWNER, so it cannot chmod them: makepkg's
    # own chmod of $srcdir fails with "Operation not permitted". Own the tree with the
    # builder before makepkg runs, then everything that needs touching is its own.
    chown -R builder:builder "/builds/$pkg"
    # makepkg's update_pkgver() rewrites pkgver= in the PKGBUILD when writable, so the
    # placeholder version (see pkbuild()) is replaced by the git-describe value.
    chmod a+w "/builds/$pkg/PKGBUILD"
    # -f overwrites leftover state from a previous, possibly incomplete run.
    # -n (not --noninteractive): this archlinux:base-devel sudo accepts short opts only
# -Syu: full upgrade, required because base-devel's cached DB is partial (openssl
# conflicts with installed curl/libarchive when the repo moved on)
    su builder -c "cd /builds/$pkg && sudo -n pacman -Syu --noconfirm && makepkg --syncdeps --noconfirm --nocheck --force"
done
# The container's non-root users map onto a host UID that is not jan (a stale tree like
# $BUILD_DIR/<pkg>/pkg/ can neither be cleaned nor inspected by jan afterwards). Map everything
# back to the container's root, which maps onto jan on the host — this keeps $BUILD_DIR usable
# for the next run.
chown -R 0:0 /builds
EOF
    # memory: this box is a shared workstation (two llama.cpp serves hold ~20GB at peak),
    # so the build container gets a hard cap that leaves room for host services. A Rust
    # build that needs more than the cap OOM-kills inside the container — makepkg reports
    # a clean failure instead of taking the whole box with it.
    # libsqlx-sys (used by sqlx) must link sqlite3 directly: without it, the proc-macro
    # .so (sqlx_macros) has undefined symbols (e.g. sqlite3_value_dup) at load time.
    podman run --rm \
        --memory=8g --memory-swap=8g \
        --network host \
        -v "$BUILD_DIR:/builds" \
        -e MAKEFLAGS="-j$NPROC" \
        -e CARGO_BUILD_JOBS="$NPROC" \
        -e RUSTFLAGS="-C target-cpu=native -C parallel-compiler -lsqlite3" \
        archlinux:base-devel \
        bash /builds/.build.sh "${REBUILD_SET[@]}" || {
        echo "=> $0: container build failed"
        exit 1
    }
    # a rootless container preserves jan's ownership of the bind mount, so `chown` (which
    # needs root) is only required when an earlier run took ownership away. Also fixes non-
    # interactive runs: sudo cannot prompt for a password without a TTY.
    if ! [ -w "$BUILD_DIR" ]; then
        log "restoring ownership of $BUILD_DIR (taken over by an earlier container run)"
        sudo chown -R "$(whoami):$(id -gn)" "$BUILD_DIR" || {
            log "ERROR: could not restore ownership of $BUILD_DIR"; exit 1;
        }
    else
        log "ownership intact (podman preserved it) — skipping chown"
    fi
    chmod 755 "$BUILD_DIR"
else
    echo "=> $0: skipping the container build (--repo-only/--reconcile)"
fi

# --- cleanup --------------------------------------------------------------------------
# (keep-src=0 removes the package dirs; their .last-built-pkgver marker and its stash in
# $MARKER_DIR are read by the publish stage after this one, so write both before the dir goes)
if [ "$KEEP_SRC" == "0" ]; then
    for aur in "${BUILD_SET[@]}"; do
        ver="$(grep -m1 '^pkgver=' "$(pkbuild_path "$aur")" 2>/dev/null | cut -d= -f2 || true)"
        if [ -f "$(pkbuild_dir "$aur")/.last-built-pkgver" ]; then
            # the marker already carries what was built last (a second full run re-reads it)
            ver="$(cat "$(pkbuild_dir "$aur")/.last-built-pkgver")"
        fi
        if [ -n "$ver" ]; then
            mkdir -p "$(pkbuild_dir "$aur")" 2>/dev/null || true
            echo "$ver" > "$(pkbuild_dir "$aur")/.last-built-pkgver"
            cp "$(pkbuild_dir "$aur")/.last-built-pkgver" "$MARKER_DIR/$aur" 2>/dev/null || true
        fi
        p="$(pkbuild_dir "$aur")"
        if [ -d "$p" ] && ! [ -w "$p" ]; then
            podman run --rm -v "$BUILD_DIR:/builds" archlinux:base-devel \
                bash -c "chmod -R u+w '$p' && chown -R 0:0 '$p'" || {
                    log "WARNING: could not restore ownership of $p"; continue;
                }
        fi
        rm -rf "$p" || log "WARNING: could not remove $p (not fatal)"
    done
fi
