#!/bin/bash

# Set PI_CODING_AGENT_DIR to the script's directory (where settings.json lives)
export PI_CODING_AGENT_DIR=$(dirname "$0")/.pi/agent
echo Using local pi-agent settings $PI_CODING_AGENT_DIR

# Model test tracking files
TESTING_FILE="testing.txt"
SUCCEEDED_FILE="succeeded.txt"
FAILED_FILE="failed.txt"

# Initialize tracking files if they don't exist
if [ ! -f "$TESTING_FILE" ]; then
    touch "$TESTING_FILE"
fi
if [ ! -f "$SUCCEEDED_FILE" ]; then
    touch "$SUCCEEDED_FILE"
fi
if [ ! -f "$FAILED_FILE" ]; then
    touch "$FAILED_FILE"
fi

echo "Starting model invocation tests..."
echo "========================================"
echo "Testing file: $TESTING_FILE"
echo "Succeeded file: $SUCCEEDED_FILE"
echo "Failed file: $FAILED_FILE"
echo "========================================"

# Read models into array for sequential processing
mapfile -t MODELS < "$TESTING_FILE"

# Move models from testing to succeeded or failed
for model in "${MODELS[@]}"; do
    echo "----------------------------------------"
    echo "Running test for: $model"
    
    # Run the test
    pi --model "$model" -p "say Hi $model"
    
    # Check exit code to determine success/failure
    if [ $? -eq 0 ]; then
        # Model succeeded - append to succeeded file
        echo "$model" >> "$SUCCEEDED_FILE"
        echo "✓ $model - SUCCEEDED"
    else
        # Model failed - append to failed file
        echo "$model" >> "$FAILED_FILE"
        echo "✗ $model - FAILED"
    fi

    # Restart llama server after each attempt
    ~/.local/bin/restart-llama-server.sh
    
    # Optional: Add a small delay between calls to prevent rate limiting
    sleep 1
done

echo "========================================"
echo "All model invocations complete."
echo "Results:"
echo "  Testing: $(wc -l < "$TESTING_FILE") models remaining"
echo "  Succeeded: $(wc -l < "$SUCCEEDED_FILE") models"
echo "  Failed: $(wc -l < "$FAILED_FILE") models"
