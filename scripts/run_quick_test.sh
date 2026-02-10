#!/bin/bash
# NASTRAN-95 Quick Test - Run a few representative examples

set -e

NASTRAN_EXE="../build/bin/nastran"

# Environment variables
export DBMEM=12000000
export OCMEM=2000000
export RFDIR=/tmp/nrf
export DIRCTY=/tmp

# Test files (representative sample)
TEST_FILES=(
    "d01000a.inp"  # Rigid format print
    "d01001a.inp"  # Static analysis (if exists)
    "d01011a.inp"  # Another test
)

echo "=========================================="
echo "NASTRAN-95 Quick Test"
echo "=========================================="

for TEST in "${TEST_FILES[@]}"; do
    INP="../examples/input/$TEST"
    if [ ! -f "$INP" ]; then
        echo "⊘ Skipping $TEST (not found)"
        continue
    fi

    BASENAME=$(basename "$TEST" .inp)
    echo -n "Testing $BASENAME ... "

    rm -f "${BASENAME}".* 2>/dev/null

    if timeout 60 "$NASTRAN_EXE" "$INP" > /dev/null 2>&1; then
        if [ -f "${BASENAME}.out" ]; then
            echo "✓ PASS"
        else
            echo "✗ FAIL (no output)"
        fi
    else
        echo "✗ FAIL (exit $?)"
    fi
done

echo "=========================================="
echo ""
echo "Output files:"
ls -lh *.out 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
