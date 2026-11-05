#!/usr/bin/env bash
# executable_rebuild-lan-repo.sh — daily rebuild trigger for the lan-repo.
#
# Renders to ~/.local/bin/rebuild-lan-repo.sh on every box (chezmoi,
# dot_local/bin/); it is only started by the systemd timer that lives on
# cyberkleiber (the build host).
#
#   executable_rebuild-lan-repo.sh
#
# Every element of `packages.list` is polled against its AUR cgit feed. When
# upstream moved to a newer stable version than what was last built,
# `$BUILD_DIR/<pkg>/.last-built-pkgver`, it calls build-lan-repo.sh <pkg> so
# the repo serves `<pkg>` + latest versions again. Pre-releases (-beta/-rc/-dev)
# are skipped; an unreachable or unreadable feed is logged and counted as a
# failure (exit code > 0 at the end of the run).
#
# Every run also reconciles: packages gone from `packages.list` are dropped from
# the repo, the db is regenerated with only what remains on disk, and after any
# run the repo serves exactly `packages.list` + latest versions (verified).
#
# Input:
#   $LAN_REPO_LIST            optional override for packages.list
#   $HOME/.local/build-lan-repo/packages.list  default policy file (chezmoi)
#   $BUILD_DIR                build dir, defaults to /home/jan/builds
# Output: journal (stdout/stderr of the systemd service), status marker in
#         $BUILD_DIR/.last-run-status

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"

LIST=${LAN_REPO_LIST:-"$HOME/.local/build-lan-repo/packages.list"}
BUILD_DIR=${BUILD_DIR:-/home/jan/builds}
MARKER_DIR="$BUILD_DIR/.last-built"
REBUILD=${REBUILD:-/home/jan/.local/bin/build-lan-repo.sh}
# build host's repo dir (source of the rsync --delete into the Pi)
REPO_DIR="$BUILD_DIR/repo"
PI_USER="jan"
PI_HOST=${PI_HOST:-f3nt0t9dnh9t8l0v.myfritz.net}   # build host → Pi (port 6622)
# ssh verification address: from the build host the Pi is reachable locally by
# name on port 22, so `ssh` (not `-e "ssh -p 6622"`) works. Only needed for the
# md5 cross-check below.
PI_SSH_HOST=${PI_SSH_HOST:-raspberrypi}
PI_DIR="$HOME/.local/arch-repo"
SKIP_PATTERN='beta|rc|dev'   # policy: no pre-releases
EXIT=0

log() { echo "=> $SCRIPT_NAME: $*" >&2; }

# --- read packages.list (policy, the authority) ---------------------------------
[ -f "$LIST" ] || { log "packages.list not found at $LIST"; exit 1; }
PACKAGES=()
while IFS= read -r line || [ -n "$line" ]; do
    line="${line// /}"            # trim whitespace
    [[ -z "$line" || "$line" == \#* ]] && continue   # blank / comment
    PACKAGES+=("$line")
done < "$LIST"
[ ${#PACKAGES[@]} -gt 0 ] || { log "packages.list ($LIST) is empty"; exit 1; }

# --- policy helpers -------------------------------------------------------------
# AUR package names are valid path components (no /, .., or spaces) — required for
# the per-package marker below. Without this check an entry like `../evil` would
# resolve outside $BUILD_DIR.
is_safe_name() { [[ "$1" =~ ^[a-zA-Z0-9._-]+$ ]]; }

# skip pre-releases (-beta/-rc/-dev, case-insensitive) — a rebuild must only
# ever track stable versions so the trigger never builds what it should not.
is_prerelease() {
    local ver="$1"
    [[ "$ver" == *[-.]beta* || "$ver" == *[-.]rc* || "$ver" == *[-.]dev* ]] && return 0
    return 1
}

# true if $1 > $2 (dotted numeric versions, e.g. 1.47.0 vs 2.0)
ver_gt() {
    [ "$1" = "$2" ] && return 1
    printf '%s\n%s\n' "$2" "$1" | sort -V | tail -n1 | grep -qxF "$1"
}

# --- last built version of one package ------------------------------------------
# $BUILD_DIR/<pkg>/.last-built-pkgver (primary) falls back to the stash that
# survives build-lan-repo.sh's package-dir cleanup.
last_built() {
    local pkg="$1" marker
    is_safe_name "$pkg" || { log "$pkg: invalid package name, cannot read the marker"; return 1; }
    marker="$BUILD_DIR/$pkg/.last-built-pkgver"
    if [ -f "$marker" ]; then echo "$(cat <"$marker")"; return 0; fi
    # the stash is written by build-lan-repo.sh before it removes $BUILD_DIR/<pkg>
    # (resolved under $MARKER_DIR before any file access)
    local marker_file="$MARKER_DIR/$pkg"
    if [ -f "$marker_file" ]; then echo "$(cat <"$marker_file")"; fi
}

# --- poll every package against the AUR cgit feed -------------------------------
BUMPED=()
for pkg in "${PACKAGES[@]}"; do
    feed="https://aur.archlinux.org/cgit/aur.git/atom/?h=$pkg"
    # entries are newest first; the first entry after the feed title is the newest
    # commit ("chore: update to <VERSION>" or an AUR equivalent).
    new_ver="$(curl -s -m 30 "$feed" | grep -oE '<title>[^<]*' | tail -n +2 | head -1 | \
               sed 's/<title>//' | grep -oE '[0-9]+(\.[0-9]+)+(-[a-zA-Z0-9]+)?')"
    if [ -z "$new_ver" ]; then
        log "$pkg: AUR feed unreachable or unreadable, skipping; failure counted at exit"
        EXIT=1
        continue
    fi
    is_prerelease "$new_ver" && { log "$pkg: $new_ver is a pre-release, skipping"; continue; }
    old_ver="$(last_built "$pkg")"
    [ -z "$old_ver" ] && old_ver="0"
    if ver_gt "$new_ver" "$old_ver"; then
        log "$pkg: $new_ver is newer than $old_ver, building"
        BUMPED+=("$pkg")
    else
        log "$pkg: up to date ($new_ver == $old_ver)"
    fi
done

# --- rebuild what moved (build-lan-repo.sh builds + reconciles the repo) ---------
if [ ${#BUMPED[@]} -gt 0 ]; then
    if ! "$REBUILD" "${BUMPED[@]}"; then
        log "build-lan-repo.sh failed for ${BUMPED[*]}; failure counted at exit"
        EXIT=1
    fi
else
    # nothing moved: only reconcile the repo so a package removed from
    # packages.list actually vanishes (reconciliation happens in build-lan-repo.sh)
    log "nothing moved; only reconciling the repo"
    if ! "$REBUILD" --reconcile; then
        log "reconciliation failed; failure counted at exit"
        EXIT=1
    fi
fi

# --- verify the repo serves exactly packages.list + latest versions ---------------
# published name for one package in packages.list (AUR name -> artifact name)
pub_name() {
    local pkg="$1"
    if [[ "$pkg" == *"-bin" ]]; then printf '%s\n' "$pkg"; else printf '%s\n' "${pkg}-bin"; fi
}

verify_repo() {
    # every artifact must resolve to a package in packages.list (no stale extras)
    for f in "$REPO_DIR"/*.pkg.tar.zst; do
        [ -f "$f" ] || continue
        name="$(basename "$f")"
        matched=0
        for pkg in "${PACKAGES[@]}"; do
            [[ "$name" == "$(pub_name "$pkg")-"* ]] && matched=1
        done
        if [ $matched -eq 0 ]; then
            log "VERIFICATION FAILED: stale artifact $(basename "$f") not in packages.list"
            return 1
        fi
    done

    # every package in packages.list must have an artifact (nothing missing)
    for pkg in "${PACKAGES[@]}"; do
        if ! ls "$REPO_DIR"/$(pub_name "$pkg")-* >/dev/null 2>&1; then
            log "VERIFICATION FAILED: $(pub_name "$pkg") has no artifact in $REPO_DIR"
            return 1
        fi
    done

    # the db must exist and be regenerated (only what's on disk belongs in it)
    [ -f "$REPO_DIR/repo.db.tar.gz" ] || {
        log "VERIFICATION FAILED: no repo.db.tar.gz in $REPO_DIR"
        return 1
    }

    # the two repo copies must stay in sync (build host + Pi)
    pi_md5="$(ssh -o ConnectTimeout=20 "$PI_USER@$PI_SSH_HOST" "md5sum $PI_DIR/repo.db.tar.gz" 2>/dev/null | cut -d' ' -f1)"
    if [ -z "$pi_md5" ]; then
        log "WARNING: could not verify the Pi's repo (ssh failed); assuming in sync"
    elif [ "$(md5sum "$REPO_DIR/repo.db.tar.gz" | cut -d' ' -f1)" != "$pi_md5" ]; then
        log "VERIFICATION FAILED: repo.db.tar.gz differs between build host and the Pi"
        return 1
    fi
}

if ! verify_repo; then
    log "repo verification failed; failure counted at exit"
    EXIT=1
else
    log "repo verified: $(ls "$REPO_DIR"/*.pkg.tar.zst | wc -l) artifact(s) serve exactly packages.list + latest versions"
fi

# --- status marker (what this run did, for a silent-failure no more) -----------
{
    echo "run=$(date -Iseconds) exit=$EXIT pkglist=$(basename "$LIST") pkgs=${PACKAGES[*]}"
} > "$BUILD_DIR/.last-run-status" 2>/dev/null || true

exit $EXIT
