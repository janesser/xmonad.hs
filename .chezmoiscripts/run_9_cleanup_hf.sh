#!/usr/bin/fish
# hf is not a mise tool (installed elsewhere on PATH); activate mise so its
# shims are on PATH, then prune the huggingface cache.
if not contains "$HOME/.local/bin" $PATH
    set -gx PATH "$HOME/.local/bin" $PATH
end
eval "$(mise activate fish)"

if mountpoint ~/.cache/huggingface/hub
    hf cache prune -y
end
