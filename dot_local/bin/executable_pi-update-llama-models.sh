#!/usr/bin/env bash
# update-llama-models.sh – refresh the `models` array in ~/.pi/agent/models.json
#   1. Get model IDs with llama cli -cl | sed -n "s/[0-9]\.\s\(.*\)/{\"id\": \"\1\"},/p"
#   2. Parse the output into a JSON array of IDs.
#   3. Replace the models list while preserving the outer schema.

set -euo pipefail

JSON_FILE="/home/jan/.pi/agent/models.json"

MODEL_IDS_JSON=$(llama cli -cl|sed -n "s/^\s*[0-9]\.\s\(.*\)/\1/p"|jq -R '{ id : . }' | jq -s)
echo $MODEL_IDS_JSON

cp "$JSON_FILE" "$JSON_FILE.old"
jq --argjson models "$MODEL_IDS_JSON" '.providers."llama-cpp".models = $models' "$JSON_FILE.old" > "$JSON_FILE"

echo "✅ models.json updated with IDs: $MODEL_IDS_JSON"

chezmoi re-add "$JSON_FILE"
