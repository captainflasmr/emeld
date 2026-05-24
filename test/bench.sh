#!/usr/bin/env bash
set -e

SELF="$(dirname "$0")"
EMELD_DIR="$(cd "$SELF/.." && pwd)"

# Run emacs in batch mode. The --eval runs emeld-diff then enters a
# sit-for loop so the async timer chain has time to complete.  When
# the tree finishes rendering, emeld--bench-report prints the timing
# table and we kill Emacs.
run_emeld() {
    local left="$1" right="$2"
    echo "Left:  $left"
    echo "Right: $right"
    echo ""

    emacs -Q --batch -L "$EMELD_DIR" -l emeld.el            \
        --eval "(setq emeld-benchmark t)"                    \
        --eval "(emeld-diff \"$left\" \"$right\")"           \
        --eval "(while t (sit-for 1)                         \
                  (when (and (not emeld--scan-process)       \
                             emeld--root-node                \
                             (emeld-node-children            \
                              emeld--root-node))             \
                    (sleep-for 0.1) (kill-emacs 0)))"         \
        2>&1 | grep -E '(Benchmark|  [a-z]|error|Error|wrong|void|Parsing|Structuring|Finalizing|Scanning|complete)'
}

small() {
    ROOT=/tmp/emeld-bench-small
    COUNT="${1:-10000}"
    A="$ROOT/a"
    B="$ROOT/b"

    echo "=== Emeld Benchmark (small: $COUNT files) ==="
    bash "$SELF/gen-test-data.sh" "$A" "$B" "$COUNT"

    # Validate
    [ -d "$A" ] || { echo "ERROR: $A not created" >&2; exit 1; }
    [ -d "$B" ] || { echo "ERROR: $B not created" >&2; exit 1; }

    run_emeld "$A" "$B"
}

large() {
    ROOT=/tmp/emeld-bench
    V1=v6.1
    V2=v5.15
    DIR1="$ROOT/linux-$V1"
    DIR2="$ROOT/linux-$V2"

    echo "=== Emeld Benchmark (large: Linux kernel $V1 vs $V2) ==="
    mkdir -p "$ROOT"

    if [ ! -d "$DIR1" ]; then
        echo "Cloning $V1 (shallow)..."
        git clone --depth 1 --branch "$V1" https://github.com/torvalds/linux.git "$DIR1"
    fi
    if [ ! -d "$DIR2" ]; then
        echo "Cloning $V2 (shallow)..."
        git clone --depth 1 --branch "$V2" https://github.com/torvalds/linux.git "$DIR2"
    fi

    [ -d "$DIR1" ] || { echo "ERROR: $DIR1 not available" >&2; exit 1; }
    [ -d "$DIR2" ] || { echo "ERROR: $DIR2 not available" >&2; exit 1; }

    run_emeld "$DIR1" "$DIR2"
}

usage() {
    echo "Usage: $0 {small [file-count] | large}"
    echo ""
    echo "  small [N]   Synthetic test with N files (default 10000)."
    echo "              Fast, no network, reproducible."
    echo "  large       Linux kernel v6.1 vs v5.15 (~80k files)."
    echo "              Requires git clone on first run."
    exit 1
}

case "${1:-small}" in
    small)  small "${2:-10000}" ;;
    large)  large ;;
    *)      usage ;;
esac
