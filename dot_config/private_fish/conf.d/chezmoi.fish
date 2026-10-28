# Guard: fish must be present for these files to have any effect.
command -v fish >/dev/null || exit 0
alias cz=chezmoi