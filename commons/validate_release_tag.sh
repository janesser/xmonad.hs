# validate_release_tag.sh — shared release-tag validation for chezmoi run scripts
#
# Validates a GitHub release tag (e.g. `v0.14.10`) before it is interpolated into a
# download URL or a filename, so it can never change the URL's scheme, host, path
# hierarchy, query, or fragment. Usage:
#
#   source "$CHEZMOI_SOURCE_DIR/commons/validate_release_tag.sh"
#   RELEASE_TAG="$(normalize_release_tag "$VERSION_LATEST")"
#   is_release_tag "$RELEASE_TAG" || { log "invalid release tag"; exit 1; }
#
# The allowlist mirrors the GitHub release tag shape, with an optional
# release-candidate suffix (`-rc1`, `-beta1`, ...), and excludes any URL delimiter,
# so a value accepted by it cannot rewrite part of the URL it is interpolated into.
# It is fail-closed: anything that is not exactly this shape is rejected before use.

RELEASE_TAG_ALLOWLIST='^v[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z][0-9A-Za-z.-]*)?$'

is_release_tag() {
    if [[ ! "$1" =~ $RELEASE_TAG_ALLOWLIST ]]; then
        echo "invalid release tag: $1" >&2
        return 1
    fi
    return 0
}

# Normalize a version into the GitHub release-tag form (v<major>.<minor>.<patch>),
# e.g. `0.14.10` -> `v0.14.10`, `v0.14.10` -> `v0.14.10`. Callers interpolate the
# returned value directly into URLs / filenames, so it must always carry the v prefix.
normalize_release_tag() {
    printf 'v%s\n' "${1#v}"
}
