#!/bin/bash

# Set PI_CODING_AGENT_DIR to the script's directory (where settings.json lives)
export PI_CODING_AGENT_DIR=$(dirname "$0")/.pi/agent
echo Using local pi-agent settings $PI_CODING_AGENT_DIR

# Model test tracking files
TESTING_FILE="testing.txt"
SUCCEEDED_FILE="succeeded.txt"
FAILED_FILE="failed.txt"

# Empty all tracking files
echo "Resetting model test files..."
echo -n "" > "$TESTING_FILE"
echo -n "" > "$SUCCEEDED_FILE"
echo -n "" > "$FAILED_FILE"

# Prefill testing.txt with models from llama cli
echo "Prefilling $TESTING_FILE from llama cli -cl output..."
llama cli -cl 2>/dev/null | sed '1d; s/^\s*[0-9]*\. //g' > "$TESTING_FILE" || echo "Warning: llama cli command failed, testing file will be empty"

echo "Ready to run model tests. Run ./run_model_tests.sh to begin."
echo "========================================"
echo "Testing: $(wc -l < "$TESTING_FILE") models"
echo "Succeeded: $(wc -l < "$SUCCEEDED_FILE") models"
echo "Failed: $(wc -l < "$FAILED_FILE") models"
echo "========================================"
