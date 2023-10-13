#!/bin/sh

echo "Running tests..."

# enumerate all files beginning with test-*
for test_file in test-*; do
    echo "Running $test_file..."

    # capture the output of cargo run
    output=$(cargo run ./$test_file 2>/dev/null)

    # check if the output contains the test string
    if echo "$output" | grep -q "eval to: t"; then
        echo "Test passed!"
    else
        echo "Test failed: Expected to find '$test_string', but got '$output'"
    fi
done
