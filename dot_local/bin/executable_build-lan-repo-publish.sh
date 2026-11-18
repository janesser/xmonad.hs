#!/usr/bin/env bash
# executable_build-lan-repo-publish.sh — publish artifacts, reconcile the repo against the
# policy file, regenerate repo.db.tar.gz and rsync it to the Raspberry Pi.
#
#   executable_build-lan-repo-publish.sh <flags>
#
# Flags are the same as for the family entry point: --build <pkg>, --keep-src, --repo-only
# and --reconcile. The package set comes from the config file written by build-lan-repo.sh
# ($BUILD_DIR/.build-lan-repo-config). This stage can also be run standalone once that config
# exists (a publish of what was left on disk after a failed or partial build run).
#
# On exit: $REPO_DIR contains the published artifacts and a db with exactly those entries,
# and the Pi's copy is in sync after the run.

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

[ ${#BUILD_SET[@]} -gt 0 ] || { log "ERROR: no packages to publish (no --build, no config)"; exit 1; }

echo "=> $0: publish stage (mode=$MODE)"
for p in "${BUILD_SET[@]}"; do echo "   -> $p"; done

mkdir -p "$REPO_DIR"

# --- publish all builds -------------------------------------------------------------------
# The published artifact is <published-name>-<version>-<arch>.pkg.tar.zst, e.g. hylki-bin-1.47.0-1-x86_64.pkg.tar.zst.
# Every build uploads its artifact: it lands in $REPO_DIR and is served by the repo after the run (via repo-add + rsync).
# A package that produced no artifact (unreachable upstream, missing dependency) is logged as a failure;
# the run continues so what did build still ends up in the repo. In full mode the whole policy list is published,
# in targeted mode only the requested package(s).
PUBLISHED=()
FAILED=()
for aur in "${BUILD_SET[@]}"; do
    pub="$(published_name "$aur")"
    # the newest artifact: from $REPO_DIR (an earlier run) or the package's build dir
    PKG="$(find_package "$pub" || true)"
    if [ -z "$PKG" ]; then
        log "ERROR: no $pub package found for $aur (build dir or repo)"
        FAILED+=("$aur")
        continue
    fi
    # the artifact may already live in $REPO_DIR (an earlier run); copying it onto itself
    # is a no-op with an error exit, so skip when it's already there.
    [ "$(dirname "$PKG")" != "$REPO_DIR" ] && cp "$PKG" "$REPO_DIR/"
    PUBLISHED+=("$aur")
    echo "=> $0: $aur -> $pub ($(basename "$PKG"))"
done
if [ ${#FAILED[@]} -gt 0 ]; then
    log "ERROR: no artifact for ${FAILED[*]} (will not be served by the repo)"
fi

# --- reconcile: what remains on disk after dropping everything not in packages.list -------
# Full mode only. Every package that leaves packages.list is dropped (plus its db entry), so
# the lan-repo serves exactly packages.list + latest versions after every full run. Targeted
# runs never reconcile: they build and upload only their package(s), which must stay served
# even when it leaves packages.list.
if [ -f "$LIST" ] && [ "$MODE" = "full" ]; then
    REMAINING=()
    for f in "$REPO_DIR"/*.pkg.tar.zst; do
        [ -f "$f" ] || continue
        name="$(basename "$f")"
        kept=0
        # the artifact is <pub>-<pkgver>-<pkgrel>-<arch>.pkg.tar.zst: keep it only when the
        # component after <pub>- is a version (a package that used to publish under a
        # different name, e.g. hylki -> hylki-bin, must be dropped instead of lingering)
        for pkg in "${BUILD_SET[@]}"; do
            [[ "$name" == "$(published_name "$pkg")-"[0-9vV]* ]] && kept=1
        done
        if [ $kept -eq 1 ]; then
            REMAINING+=("$f")
        else
            rm -f "$f"
            log "dropped stale artifact $(basename "$f") (not in the policy)"
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

# --- regenerate the db (repo-add is an Arch tool — run it in the container) --------------
# The db must contain ONLY what is on disk: a stale db (entries for packages that are not on
# disk) makes pacman fail when it tries to download missing files.
rm -f "$REPO_DIR/repo.db.tar.gz" "$REPO_DIR/repo.db.tar.gz.old" \
      "$REPO_DIR/repo.files.tar.gz" "$REPO_DIR/repo.files.tar.gz.old"
ARGS=()
for f in "${REMAINING[@]}"; do ARGS+=("/repo/$(basename "$f")"); done
if [ ${#ARGS[@]} -gt 0 ]; then
    podman run --rm \
        -v "$REPO_DIR:/repo" \
        archlinux:base-devel \
        bash -c 'pacman -Sy --noconfirm pacman-contrib && repo-add /repo/repo.db.tar.gz "$@"' bash "${ARGS[@]}" 2>&1 | tail -5 || {
        echo "=> $0: repo-add failed (unsigned package? check SIGNKEY in makepkg.conf)"
        exit 1
    }
fi
if [ ! -f "$REPO_DIR/repo.db.tar.gz" ]; then
    log "ERROR: repo-add did not produce a db (no artifacts on disk? see logs)"
    exit 1
fi
log "repo.db regenerated ($(ls "$REPO_DIR"/*.pkg.tar.zst | wc -l) artifact(s) on disk)"

# --- sync to the Pi ---------------------------------------------------------------------
echo "=> $0: rsyncing repo to pi@$PI_HOST:$PI_DIR"
# the Pi's SSH listener is on 6622 (myfritz), not the default 22
rsync -a --delete -e "ssh -p 6622" "$REPO_DIR/" "$PI_USER@$PI_HOST:$PI_DIR/" || {
    log "ERROR: rsync to the Pi failed"
    exit 1
}
echo "=> $0: repo synced to pi@$PI_HOST:$PI_DIR"

# --- housekeeping ------------------------------------------------------------------------
# mark what was last built ($BUILD_DIR/<pkg>/.last-built-pkgver, with a stash copy that
# survives the package dir cleanup below) and remove the AUR clones.
if [ ${#PUBLISHED[@]} -gt 0 ]; then
    mkdir -p "$MARKER_DIR"
    for aur in "${PUBLISHED[@]}"; do
        ver="$(grep -m1 '^pkgver=' "$(pkbuild_path "$aur")" 2>/dev/null | cut -d= -f2 || true)"
        if [ -z "$ver" ]; then
            # primary marker from the build stage (kept in $BUILD_DIR/<pkg> while it exists)
            [ -f "$(pkbuild_dir "$aur")/.last-built-pkgver" ] && \
                ver="$(cat "$(pkbuild_dir "$aur")/.last-built-pkgver")"
        fi
        if [ -z "$ver" ]; then
            # the stash that survives the package dir cleanup (kept by the build stage)
            [ -f "$MARKER_DIR/$aur" ] && ver="$(cat "$MARKER_DIR/$aur")"
        fi
        if [ -z "$ver" ]; then
            # fall back to the version baked into the artifact's name on disk
            pat="$REPO_DIR/$(published_name "$aur")-*"
            ver="$(ls "$pat" 2>/dev/null | sed 's|.*/||' | grep -m1 -oE '[0-9]+(\.[0-9]+)+(-[a-zA-Z0-9]+)?')"
        fi
        if [ -z "$ver" ]; then
            log "WARNING: could not determine pkgver for $aur (marker untouched)"
            continue
        fi
        # the primary marker is $BUILD_DIR/<pkg>/.last-built-pkgver; the copy in
        # $MARKER_DIR ($MARKER_DIR/$aur) is the stash that survives removing the
        # package dir with --keep-src (the trigger reads the primary first, falls
        # back to this one)
        mkdir -p "$(pkbuild_dir "$aur")" 2>/dev/null || true
        echo "$ver" > "$(pkbuild_dir "$aur")/.last-built-pkgver"
        cp "$(pkbuild_dir "$aur")/.last-built-pkgver" "$MARKER_DIR/$aur" 2>/dev/null || true
        log "marked $aur built: $ver ($MARKER_DIR/$aur)"
    done
fi

# a package that produced no artifact is reported but does not abort the run above:
# the repo still needs to be reconciled, the db regenerated and rsynced.
if [ ${#FAILED[@]} -gt 0 ]; then
    log "FAILED: no artifacts for ${FAILED[*]}"
    exit 1
fi

echo "=> $0: done. omarchy: pacman -Sy && pacman -S <package> (from http://<pi>:8080/)"
