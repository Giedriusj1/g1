#!/bin/sh

echo "Running tests..."

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# enumerate all files beginning with test-*
for test_file in test-*; do

    # echo "Running $test_file..."

    # capture the output of cargo run
    output=$(cargo run ./$test_file 2>/dev/null)

    # check if the output contains the test string
    if echo "$output" | grep -q "eval to: t"; then
        echo -e "${GREEN}[OK]    ${NC} $test_file"
    else
        echo -e "${RED}[FAILED]${NC} $test_file: Expected to find '$test_string', but got '$output'"
    fi
done
