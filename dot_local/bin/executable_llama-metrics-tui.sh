#!/bin/bash
# llama-metrics-tui.sh — btop-style TUI view onto the llama.cpp metrics
# collected by Prometheus from the two routers (:8081 ornith, :8082 little
# brother).
#
# The dashboard config is ~/.config/grom/llama-metrics.yaml (managed by
# chezmoi). This script is a thin wrapper around `grom run` so the TUI can be
# started with a single command:
#
#   llama-metrics-tui.sh [refresh_seconds] [theme]
#
# The refresh arg is passed through to grom's dashboard config (`refresh: 10s`)
# only as a hint — grom reads its own config. The theme arg selects one of
# grom's built-in themes (tokyo-night is the default).
#
# grom itself is installed by chezmoi (run_once_5_aitools_4grom.sh) from the
# upstream releases page. If grom is missing, fall back to the prometheus-cli
# style table (curl based) so the TUI still works.
set -u

REFRESH="${1:-10}"
THEME="${2:-tokyo-night}"

if command -v grom >/dev/null 2>&1; then
    exec grom run \
        --config "$HOME/.config/grom/llama-metrics.yaml" \
        --prom http://127.0.0.1:9090 \
        --theme "$THEME"
fi

# Fallback: plain curl table (no grom available).
clear
echo "llama.cpp metrics (prometheus 127.0.0.1:9090) — no grom, falling back to curl"
echo "================================================================"
for m in "llamacpp:prompt_tokens_total|prompt_tokens_total" "llamacpp:n_decode_total|n_decode_total" "llamacpp:requests_processing|requests_processing" "llamacpp:tokens_predicted_total|tokens_predicted_total"; do
    q="${m%%|*}"
    name="${m##*|}"
    q=$(printf '%s' "$q" | python3 -c 'import sys,urllib.parse;print(urllib.parse.quote(sys.stdin.read().strip(), safe=""))')
    curl -s "http://127.0.0.1:9090/api/v1/query?query=$q" > "$HOME/.tmp-metrics.json"
    python3 - "$name" "$HOME/.tmp-metrics.json" <<'PY'
import json, sys
with open(sys.argv[2]) as f:
    data = json.load(f)['data']['result']
for t in data:
    m = t['metric']
    print(f"{m['job']:<22} {m['instance']:<18} {sys.argv[1]:<24} {t['value'][1]:>12}")
PY
    rm -f "$HOME/.tmp-metrics.json"
    echo "----------------------------------------------------------------"
done
