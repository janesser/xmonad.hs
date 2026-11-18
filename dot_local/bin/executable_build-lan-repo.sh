#!/usr/bin/env bash
# executable_build-lan-repo.sh — build AUR packages in a memory-capped Arch container on the
# fleet build host (cyberkleiber) and publish them to the lan_repo on the Raspberry Pi.
#
#   executable_build-lan-repo.sh [packages.list] [--build <pkg>...] [pkg1 pkg2...]
#                                [--repo-only] [--keep-src] [--reconcile]
#
# Two mutually exclusive modes, determined by how the script is called:
#
#   full (default, no package name): rebuild everything in packages.list — every
#       entry of the policy file is cloned, built and reconciled against it, so
#       after a run the repo serves exactly `packages.list` + latest versions.
#       This is the mode of the daily rebuild trigger (16:00, systemd timer).
#
#   targeted (`--build <pkg>` or one or more bare package names): build and
#       upload ONLY the named package(s) — clone, patch, build in the container,
#       publish their artifact to the repo and rsync it to the Pi. The policy
#       file is never consulted (a targeted build must always be uploaded even
#       when the target leaves packages.list), and nothing else in the repo is
#       touched.
#
#   [packages.list]    Policy file (default: ~/.local/build-lan-repo/packages.list,
#                      the chezmoi-rendered one): only consulted in full mode.
#                      everything on it must be served by the lan-repo, nothing
#                      else. Packages that leave the list are dropped.
#   [pkg1 pkg2...]     AUR names to build (overrides the policy list, targeted).
#   --build <name>     same as giving the name (targeted build).
#   --package <name>   same as --build <name> (backwards compatible flag).
#   --reconcile        skip all builds: drop everything not on packages.list and
#                      regenerate the db (the rebuild trigger uses this when nothing
#                      moved so a package removed from packages.list really vanishes).
#   --repo-only        skip the container build; only repo-add + rsync.
#   --keep-src         keep the AUR clones in $BUILD_DIR instead of removing them.
#
# Flow: clone PKGBUILDs -> makepkg in one memory-capped Arch container
# (archlinux:base-devel) -> publish every built artifact into $REPO_DIR
# -> (full mode only) reconcile against packages.list -> repo-add -> rsync to
# the Pi -> mark last-built versions. Every build uploads its artifact: it lands
# in $REPO_DIR and is served by the repo after the run. The lan-repo serves
# exactly packages.list + latest versions after every full run, and only the
# targeted package(s) are uploaded by a targeted run.
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
#
# This script is the family's entry point: it parses arguments, decides the mode
# and the package set, writes $BUILD_DIR/.build-lan-repo-config (mode + flags +
# packages) and dispatches to the sibling stages (build-lan-repo-build.sh,
# build-lan-repo-publish.sh), each of which can also run standalone once that
# config exists. The whole family is podman-native: every container invocation
# calls `podman` directly, never through /usr/bin/docker's emulation layer.

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
source "$SCRIPT_DIR/build-lan-repo-common.sh"

set -uo pipefail

BUILD_HOST="${BUILD_HOST:-cyberkleiber}"
LIST="${LAN_REPO_LIST:-$HOME/.local/build-lan-repo/packages.list}"
TARGETED=()     # packages requested via --build (targeted mode)
PACKAGES=()
POSITIONAL=()
RECONCILE_ONLY=0
REPO_ONLY=0
KEEP_SRC=0
MODE="full"

while [ $# -gt 0 ]; do
    case "$1" in
        --host)        BUILD_HOST="$2"; shift 2 ;;
        --host=*)      BUILD_HOST="${1#*=}"; shift 2 ;;
        --pi)          PI_HOST="$2"; shift 2 ;;
        --pi=*)        PI_HOST="${1#*=}"; shift 2 ;;
        --build)       [[ $2 =~ ^[a-zA-Z0-9._-]+$ ]] || { log "ERROR: invalid package name: $2"; exit 1; }
                    TARGETED+=("$2"); shift 2 ;;
        --build=*)     [[ ${1#*=} =~ ^[a-zA-Z0-9._-]+$ ]] || { log "ERROR: invalid package name: $1"; exit 1; }
                    TARGETED+=("${1#*=}"); shift ;;
        --package)     [[ $2 =~ ^[a-zA-Z0-9._-]+$ ]] || { log "ERROR: invalid package name: $2"; exit 1; }
                    TARGETED+=("$2"); shift 2 ;;   # backwards compatible --build
        --package=*)   [[ ${1#*=} =~ ^[a-zA-Z0-9._-]+$ ]] || { log "ERROR: invalid package name: $1"; exit 1; }
                    TARGETED+=("${1#*=}"); shift ;;
        --keep-src)    KEEP_SRC=1; shift ;;
        --repo-only)   REPO_ONLY=1; shift ;;
        --reconcile)   RECONCILE_ONLY=1; shift ;;
        -h|--help)     head -8 "$0"; exit 0 ;;
        *)             POSITIONAL+=("$1"); shift ;;
    esac
done

# bare package names are targeted builds too (e.g. build-lan-repo.sh hylki)
TARGETED+=("${POSITIONAL[@]+"${POSITIONAL[@]}"}")

# --- packages.list: the policy (the authority for what the repo serves) ---------------
PACKAGES_FROM_LIST=()
if [ -f "$LIST" ]; then
    while IFS= read -r line || [ -n "$line" ]; do
        line="${line// /}"                # trim whitespace
        [[ -z "$line" || "$line" == \#* ]] && continue   # blank / comment
        PACKAGES_FROM_LIST+=("$line")
    done < "$LIST"
fi

# --- the package set and the mode: full rebuild or targeted build ---------------------
if [ ${#TARGETED[@]} -gt 0 ]; then MODE="targeted"; fi

BUILD_SET=()
if [ "$MODE" = "targeted" ]; then
    [ ${#TARGETED[@]} -gt 0 ] || { log "ERROR: --build requires a package name"; exit 1; }
    BUILD_SET=("${TARGETED[@]}")
else
    if [ ! -f "$LIST" ]; then
        log "ERROR: no packages to build (no arguments and $LIST not found)"
        exit 1
    fi
    [ ${#PACKAGES_FROM_LIST[@]} -gt 0 ] || {
        log "ERROR: $LIST is empty"; exit 1; }
    BUILD_SET=("${PACKAGES_FROM_LIST[@]}")
fi

echo "=> $0: $MODE rebuild of ${#BUILD_SET[@]} package(s) on $BUILD_HOST, publishing to pi@$PI_HOST"
for p in "${BUILD_SET[@]}"; do echo "   -> $p"; done

# --- dispatch config (mode + flags + package set for the stages) ------------------------
# The build stage reads this file and writes $BUILD_DIR/.build.sh; the publish stage
# reads it to reconcile the repo and regenerate the db. A standalone run of either stage
# works once this file exists — or via its own --build / --reconcile / --keep-src flags.
mkdir -p "$BUILD_DIR"
{
    echo "MODE=$MODE"
    echo "KEEP_SRC=$KEEP_SRC"
    echo "REPO_ONLY=$REPO_ONLY"
    echo "RECONCILE_ONLY=$RECONCILE_ONLY"
    printf '%s\n' "${BUILD_SET[@]}"
} > "$BUILD_DIR/.build-lan-repo-config"

# --- the build stages live next to this script -------------------------------------------
FAMILY_DIR="$SCRIPT_DIR"

# --- run the build stage (skipped when only reconciling or repo-add is requested) --------
if [ "$RECONCILE_ONLY" != "1" ] && [ "$REPO_ONLY" != "1" ]; then
    if ! "$FAMILY_DIR/build-lan-repo-build.sh" "$@"; then
        log "ERROR: build stage failed"
        exit 1
    fi
fi

# --- run the publish stage (always runs at the end) ----------------------------------------
if ! "$FAMILY_DIR/build-lan-repo-publish.sh" "$@"; then
    log "ERROR: publish stage failed"
    exit 1
fi

echo "=> $0: done. omarchy: pacman -Sy && pacman -S <package> (from http://<pi>:8080/)"
