#!/bin/bash
# NASTRAN-95 Example Test Runner
# Runs all example input files and reports results

set -e

# Parse command-line arguments
FORCE=false
while [[ $# -gt 0 ]]; do
    case $1 in
        --force|-f)
            FORCE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--force]"
            echo ""
            echo "Options:"
            echo "  --force, -f    Re-run all tests even if output exists"
            echo "  --help, -h     Show this help message"
            echo ""
            echo "By default, skips tests that already have output files."
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Configuration
NASTRAN_EXE="../build/bin/nastran"
EXAMPLES_DIR="../examples/input"
RESULTS_FILE="test_results.txt"
SUMMARY_FILE="test_summary.txt"

# Environment variables
export DBMEM=12000000
export OCMEM=2000000
export RFDIR=/tmp/nrf
export DIRCTY=/tmp

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if NASTRAN executable exists
if [ ! -f "$NASTRAN_EXE" ]; then
    echo -e "${RED}Error: NASTRAN executable not found at $NASTRAN_EXE${NC}"
    echo "Please build NASTRAN first: cd ../build && cmake --build ."
    exit 1
fi

# Initialize results
> "$RESULTS_FILE"
> "$SUMMARY_FILE"

TOTAL=0
PASSED=0
FAILED=0
WARNINGS=0

echo "=========================================="
echo "NASTRAN-95 Example Test Runner"
echo "=========================================="
echo "Executable: $NASTRAN_EXE"
echo "Examples:   $EXAMPLES_DIR"
echo "Output:     $(pwd)"
echo "=========================================="
echo ""

# Find all .inp files
INP_FILES=$(find "$EXAMPLES_DIR" -maxdepth 1 -name "*.inp" -type f | sort)

if [ -z "$INP_FILES" ]; then
    echo -e "${RED}No .inp files found in $EXAMPLES_DIR${NC}"
    exit 1
fi

# Count total files
TOTAL=$(echo "$INP_FILES" | wc -l)

echo "Found $TOTAL example files"
if [ "$FORCE" = true ]; then
    echo "Mode: Force re-run (--force)"
else
    echo "Mode: Skip existing outputs (use --force to re-run)"
fi
echo ""

SKIPPED=0

# Process each input file
for INP_FILE in $INP_FILES; do
    BASENAME=$(basename "$INP_FILE" .inp)

    # Check if output already exists (unless --force)
    if [ "$FORCE" = false ] && [ -f "${BASENAME}.out" ] && [ -f "${BASENAME}.log" ]; then
        echo -e "${BLUE}↷${NC} $BASENAME (skipped - output exists)"
        echo "$BASENAME: SKIP" >> "$RESULTS_FILE"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    echo -n "Testing $BASENAME ... "

    # Clean previous outputs
    rm -f "${BASENAME}".{out,log,dic,pun,plt,nptp} 2>/dev/null

    # Run NASTRAN with timeout
    START_TIME=$(date +%s)

    if timeout 300 "$NASTRAN_EXE" "$INP_FILE" > /dev/null 2>&1; then
        EXIT_CODE=0
    else
        EXIT_CODE=$?
    fi

    END_TIME=$(date +%s)
    ELAPSED=$((END_TIME - START_TIME))

    # Check results
    if [ $EXIT_CODE -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC} (>300s)"
        echo "$BASENAME: TIMEOUT" >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
    elif [ $EXIT_CODE -eq 0 ]; then
        # Check if output files were created
        if [ -f "${BASENAME}.out" ] && [ -f "${BASENAME}.log" ]; then
            # Check for fatal errors
            if grep -q "FATAL" "${BASENAME}.out" 2>/dev/null; then
                echo -e "${YELLOW}WARNING${NC} (${ELAPSED}s) - Fatal errors in output"
                echo "$BASENAME: WARNING - Fatal errors" >> "$RESULTS_FILE"
                WARNINGS=$((WARNINGS + 1))
            else
                echo -e "${GREEN}PASS${NC} (${ELAPSED}s)"
                echo "$BASENAME: PASS" >> "$RESULTS_FILE"
                PASSED=$((PASSED + 1))
            fi
        else
            echo -e "${RED}FAIL${NC} (${ELAPSED}s) - Missing output files"
            echo "$BASENAME: FAIL - Missing output" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
        fi
    else
        echo -e "${RED}FAIL${NC} (${ELAPSED}s) - Exit code $EXIT_CODE"
        echo "$BASENAME: FAIL - Exit $EXIT_CODE" >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
    fi
done

echo ""
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Total:    $TOTAL"
if [ $SKIPPED -gt 0 ]; then
    echo -e "Skipped:  ${BLUE}$SKIPPED${NC}"
    RAN=$((TOTAL - SKIPPED))
    echo "Ran:      $RAN"
fi
echo -e "Passed:   ${GREEN}$PASSED${NC}"
echo -e "Warnings: ${YELLOW}$WARNINGS${NC}"
echo -e "Failed:   ${RED}$FAILED${NC}"
echo "=========================================="

# Write summary
{
    echo "NASTRAN-95 Example Test Summary"
    echo "Date: $(date)"
    echo "Total: $TOTAL"
    echo "Passed: $PASSED"
    echo "Warnings: $WARNINGS"
    echo "Failed: $FAILED"
    echo ""
    echo "Details:"
    cat "$RESULTS_FILE"
} > "$SUMMARY_FILE"

echo ""
echo "Results saved to:"
echo "  - $RESULTS_FILE (detailed results)"
echo "  - $SUMMARY_FILE (summary)"
echo ""

# Show failed tests
if [ $FAILED -gt 0 ]; then
    echo "Failed tests:"
    grep "FAIL" "$RESULTS_FILE" | cut -d: -f1
    echo ""
fi

# Exit with appropriate code
if [ $FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi
