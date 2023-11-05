#!/bin/sh

echo "Running tests..."

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Function to run tests and print the result
run_test() {
    test_file="$1"
    output=$(cargo run ./$test_file 2>/dev/null)
    if echo "$output" | grep -q "eval to: t"; then
        echo -e "${GREEN}[OK]    ${NC} $test_file"
    else
        echo -e "${RED}[FAILED]${NC} $test_file: Expected to find 'eval to: t', but got '$output'"
    fi
}

# Find all files beginning with test-* and run tests concurrently
for test_file in ./tests/test-*; do
    run_test "$test_file" &
done

# Wait for all background processes to finish
wait
