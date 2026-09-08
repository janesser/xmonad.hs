# mise shell integration (replaces the old asdf.fish).
# Puts mise's shims on PATH and updates tool versions as you cd between dirs.
if not contains "$HOME/.local/bin" $PATH
    set -gx --prepend PATH "$HOME/.local/bin"
end

if command -v mise >/dev/null 2>&1
    eval "$(mise activate fish)"
end
