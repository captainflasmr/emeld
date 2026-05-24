#!/usr/bin/env bash
# Generate two directory trees with controlled file counts for benchmarking.
# Usage: gen-test-data.sh <dir-a> <dir-b> [file-count]
#   file-count defaults to 10000; split evenly across 5 subdirs.
set -e

A="$1"
B="$2"
COUNT="${3:-10000}"

if [ -z "$A" ] || [ -z "$B" ]; then
    echo "Usage: $0 <dir-a> <dir-b> [file-count]" >&2
    exit 1
fi

PER_DIR=$((COUNT / 5))
SUBDIRS=("src" "lib" "tests" "docs" "assets")

echo "Generating $COUNT files across ${#SUBDIRS[@]} subdirs..."
rm -rf "$A" "$B"

for dir in "$A" "$B"; do
    mkdir -p "$dir"
    for sub in "${SUBDIRS[@]}"; do
        mkdir -p "$dir/$sub/deep/nested/path"
        for f in $(seq 1 $PER_DIR); do
            echo "content $f" > "$dir/$sub/deep/nested/path/file_$(printf '%05d' $f).txt"
        done
    done
done

# Make side B slightly different: change a subset of files
echo "Introducing variations in side B..."
# 5% modified
for f in $(seq 1 $((PER_DIR * 5 / 100))); do
    echo "modified content $f" > "$B/${SUBDIRS[$((f % 5))]}/deep/nested/path/file_$(printf '%05d' $f).txt"
done
# 2% left-only files (add to A only)
for f in $(seq 1 $((PER_DIR * 2 / 100))); do
    echo "new $f" > "$A/${SUBDIRS[$((f % 5))]}/deep/nested/path/left_only_$(printf '%05d' $f).txt"
done
# 2% right-only files (add to B only)
for f in $(seq 1 $((PER_DIR * 2 / 100))); do
    echo "new $f" > "$B/${SUBDIRS[$((f % 5))]}/deep/nested/path/right_only_$(printf '%05d' $f).txt"
done

echo "Generated:"
echo "  A: $(find "$A" -type f | wc -l) files in $A"
echo "  B: $(find "$B" -type f | wc -l) files in $B"
echo "  Modified: $((PER_DIR * 5 / 100 * 5))"
echo "  Left-only: $((PER_DIR * 2 / 100 * 5))"
echo "  Right-only: $((PER_DIR * 2 / 100 * 5))"
echo "  Same: $((COUNT - PER_DIR * 5 / 100 * 5 - PER_DIR * 2 / 100 * 5))"
