#!/usr/bin/env bash
set -e

SELF="$(dirname "$0")"
EMELD_DIR="$(cd "$SELF/.." && pwd)"

# Behaviour knobs (env vars):
#   EMELD_BENCH_RECORD=0   Do not append to the benchmark log (just print).
#   EMELD_BENCH_SAME=1     Also run the identical-files (S) pass and record it.
#   EMELD_BENCH_FILE=path  Benchmark log to append to
#                          (default: BENCHMARKS.org beside emeld.el).
RECORD="${EMELD_BENCH_RECORD:-1}"
SAME="${EMELD_BENCH_SAME:-0}"
BENCH_FILE="${EMELD_BENCH_FILE:-$EMELD_DIR/BENCHMARKS.org}"

# Run emacs in batch mode.  The driver runs emeld-diff, waits (via a
# sit-for loop that lets the async timer chain run) until the scan reaches
# the 'done phase, optionally runs the identical-files pass the same way,
# then appends the collected runs to the benchmark log via
# emeld-benchmark-record and exits.  emeld--bench-report prints each phase
# table as it completes; emeld-benchmark-record prints a "Recorded ..." line.
run_emeld() {
    local left="$1" right="$2"
    echo "Left:   $left"
    echo "Right:  $right"
    if [ "$SAME" = 1 ]; then
        echo "Pass:   initial + identical-files (S)"
    else
        echo "Pass:   initial"
    fi
    [ "$RECORD" = 1 ] && echo "Record: $BENCH_FILE"
    echo ""

    emacs -Q --batch -L "$EMELD_DIR" -l emeld.el                          \
        --eval "(progn
                  (setq emeld-benchmark t)
                  (setq emeld-benchmark-file \"$BENCH_FILE\")
                  (emeld-diff \"$left\" \"$right\")
                  (let ((buf (current-buffer))
                        (deadline (+ (float-time) 1800)))
                    (cl-labels
                        ((wait-done ()
                           (while (and (not (eq (buffer-local-value
                                                 'emeld--status-phase buf)
                                                'done))
                                       (< (float-time) deadline))
                             (sit-for 0.2))))
                      (wait-done)
                      (when (= $SAME 1)
                        (with-current-buffer buf
                          (setq emeld-show-same t)
                          (emeld--scan-same-async buf))
                        (sit-for 0.3)
                        (wait-done))
                      (when (= $RECORD 1)
                        (with-current-buffer buf (emeld-benchmark-record)))
                      (kill-emacs 0))))"                                   \
        2>&1 | grep -E '(Benchmark|  [a-z]|error|Error|wrong|void|Recorded|Scanning|complete)'
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
    # V1=v6.1
    # V2=v5.15
    V1=v7.0
    V2=v6.19
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
    echo "  large       Linux kernel v7.0 vs v6.19 (~80k files)."
    echo "              Requires git clone on first run."
    echo ""
    echo "Each run appends to the benchmark log (BENCHMARKS.org) by default."
    echo "Env vars:"
    echo "  EMELD_BENCH_RECORD=0   print only, do not append to the log"
    echo "  EMELD_BENCH_SAME=1     also run + record the identical-files (S) pass"
    echo "  EMELD_BENCH_FILE=PATH  append to PATH instead of the default log"
    exit 1
}

case "${1:-small}" in
    small)  small "${2:-10000}" ;;
    large)  large ;;
    *)      usage ;;
esac
