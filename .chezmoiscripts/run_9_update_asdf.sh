#!/bin/bash

export PATH=~/go/bin:$PATH # in case not yet set

asdf plugin update --all || echo "warning: some plugins failed to update"

resolve_version() {
    local plugin="$1" v
    case "$plugin" in
        java)
            # `asdf latest java` needs a distro prefix; Temurin is the default.
            # Strip the -musl suffix: musl builds target Alpine and won't run on
            # standard glibc hosts (e.g. Ubuntu).
            v="$(asdf latest java temurin 2>/dev/null | sed 's/-musl//')"
            ;;
        nodejs)
            # Modern asdf: asdf cmd nodejs resolve lts (falls back to plain latest).
            v="$(asdf cmd nodejs resolve lts 2>/dev/null)"
            # Only fall back if the raw output was not a clean version string
            # (e.g. older asdf that can't dispatch the command prints help text).
            printf '%s' "$v" | grep -qE '^[v]?[0-9]' \
                || v="$(asdf latest nodejs 2>/dev/null)"
            ;;
        *)
            v="$(asdf latest "$plugin" 2>/dev/null)"
            ;;
    esac
    # Keep only clean version strings (temurin-26.0.2+101, v22.13.1, 26.8.1).
    # Anything else (help text, error output) is treated as "no version".
    printf '%s' "$v" | tr -d '[:space:]' | grep -E '^(temurin-|v?[0-9])' || true
}

# 3. Install the newest version for every registered plugin.
for plugin in $(asdf plugin list | awk '{print $1}'); do
    version="$(resolve_version "$plugin")"
    if [ -z "$version" ]; then
        echo "skip $plugin: no baseline set (add it to .tool-versions)"
        continue
    fi
    echo "update $plugin -> $version"
    asdf install "$plugin" "$version" || echo "warning: $plugin failed to update"
    asdf set "$plugin" "$version" || echo "warning: $plugin failed to set"
done
