#! /bin/sh

echo "Running tests..."

# enumerate all files beginning with test-*
for test_file in test-*; do
    echo "Running $test_file..."
    cargo run ./$test_file
done
