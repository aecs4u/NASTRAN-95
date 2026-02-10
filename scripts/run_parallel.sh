#!/bin/bash
# NASTRAN-95 Parallel Test Runner with proper job isolation

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
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Configuration
NASTRAN_EXE="../build/bin/nastran"
EXAMPLES_DIR="../examples/input"
RESULTS_DIR="./parallel_results"
MAX_JOBS=2  # Reduced to 2 for stability

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Check executable
if [ ! -f "$NASTRAN_EXE" ]; then
    echo -e "${RED}Error: NASTRAN executable not found${NC}"
    exit 1
fi

# Convert to absolute paths (important for cd operations in run_test)
NASTRAN_EXE=$(realpath "$NASTRAN_EXE")
EXAMPLES_DIR=$(realpath "$EXAMPLES_DIR")

echo "=========================================="
echo "NASTRAN-95 Parallel Test Runner"
echo "=========================================="
echo "Parallel jobs: $MAX_JOBS"
echo "Executable: $NASTRAN_EXE"
if [ "$FORCE" = true ]; then
    echo "Mode: Force re-run (--force)"
else
    echo "Mode: Skip existing outputs (use --force to re-run)"
fi
echo "=========================================="
echo ""

# Create results directory if force mode, or preserve existing
if [ "$FORCE" = true ]; then
    rm -rf "$RESULTS_DIR"
fi
mkdir -p "$RESULTS_DIR"

# Find all .inp files
mapfile -t INP_FILES < <(find "$EXAMPLES_DIR" -maxdepth 1 -name "*.inp" -type f | sort)

if [ ${#INP_FILES[@]} -eq 0 ]; then
    echo -e "${RED}No .inp files found${NC}"
    exit 1
fi

TOTAL=${#INP_FILES[@]}
echo "Found $TOTAL example files"
echo ""

# Function to run a single test with isolation
run_test() {
    local INP_FILE=$1
    local BASENAME=$(basename "$INP_FILE" .inp)
    local JOB_DIR="$RESULTS_DIR/$BASENAME"

    # Create isolated directory for this job
    mkdir -p "$JOB_DIR"
    cd "$JOB_DIR"

    # Set unique temp file
    export TMPDIR="$JOB_DIR/tmp"
    mkdir -p "$TMPDIR"

    # Environment with unique temp file paths
    export DBMEM=12000000
    export OCMEM=2000000
    export RFDIR=/tmp/nrf
    export DIRCTY="$TMPDIR"

    local START=$(date +%s)
    local STATUS="FAIL"

    # Run NASTRAN with output captured
    if timeout 300 "$NASTRAN_EXE" "$INP_FILE" > run.log 2>&1; then
        if [ -f "${BASENAME}.out" ]; then
            if grep -q "FATAL" "${BASENAME}.out" 2>/dev/null; then
                STATUS="WARNING"
            else
                STATUS="PASS"
            fi
        fi
    elif [ $? -eq 124 ]; then
        STATUS="TIMEOUT"
    elif [ $? -eq 139 ]; then
        STATUS="SEGFAULT"
        echo "Segmentation fault detected" >> run.log
    fi

    local END=$(date +%s)
    local ELAPSED=$((END - START))

    # Write result
    echo "$STATUS" > status.txt
    echo "$ELAPSED" > time.txt

    # Clean up temp files
    rm -rf "$TMPDIR"
}

export -f run_test
export NASTRAN_EXE
export RESULTS_DIR

# Run tests in parallel with job control
PIDS=()
RUNNING=0
INDEX=0

SKIPPED=0

for INP_FILE in "${INP_FILES[@]}"; do
    BASENAME=$(basename "$INP_FILE" .inp)

    # Check if output already exists (unless --force)
    if [ "$FORCE" = false ]; then
        JOB_DIR="$RESULTS_DIR/$BASENAME"
        if [ -f "${BASENAME}.out" ] && [ -f "${BASENAME}.log" ]; then
            echo -e "\r${BLUE}↷${NC} $BASENAME (skipped - output exists)                    "

            # Create status file for skipped test
            mkdir -p "$JOB_DIR"
            echo "SKIP" > "$JOB_DIR/status.txt"
            echo "0" > "$JOB_DIR/time.txt"

            SKIPPED=$((SKIPPED + 1))
            INDEX=$((INDEX + 1))
            continue
        fi
    fi

    # Wait if we have MAX_JOBS running
    while [ $RUNNING -ge $MAX_JOBS ]; do
        for i in "${!PIDS[@]}"; do
            if ! kill -0 "${PIDS[$i]}" 2>/dev/null; then
                unset 'PIDS[i]'
                RUNNING=$((RUNNING - 1))
            fi
        done
        sleep 0.5
    done

    # Start new job in background
    (run_test "$INP_FILE") &
    PIDS+=($!)
    RUNNING=$((RUNNING + 1))
    INDEX=$((INDEX + 1))

    echo -ne "\rStarted: $INDEX/$TOTAL tests"
done

# Wait for all jobs to complete
echo ""
echo "Waiting for all jobs to complete..."
wait

echo ""
echo "=========================================="
echo "Collecting Results"
echo "=========================================="

# Collect and display results
PASSED=0
WARNINGS=0
FAILED=0
SEGFAULTS=0
TIMEOUTS=0

for INP_FILE in "${INP_FILES[@]}"; do
    BASENAME=$(basename "$INP_FILE" .inp)
    JOB_DIR="$RESULTS_DIR/$BASENAME"

    if [ -f "$JOB_DIR/status.txt" ]; then
        STATUS=$(cat "$JOB_DIR/status.txt")
        TIME=$(cat "$JOB_DIR/time.txt" 2>/dev/null || echo "?")

        case "$STATUS" in
            PASS)
                echo -e "${GREEN}✓${NC} $BASENAME (${TIME}s)"
                PASSED=$((PASSED + 1))
                ;;
            WARNING)
                echo -e "${YELLOW}⚠${NC} $BASENAME (${TIME}s)"
                WARNINGS=$((WARNINGS + 1))
                ;;
            SKIP)
                echo -e "${BLUE}↷${NC} $BASENAME (skipped)"
                # Don't count skipped in any category
                ;;
            TIMEOUT)
                echo -e "${RED}⏱${NC} $BASENAME (timeout)"
                TIMEOUTS=$((TIMEOUTS + 1))
                FAILED=$((FAILED + 1))
                ;;
            SEGFAULT)
                echo -e "${RED}💥${NC} $BASENAME (segfault)"
                SEGFAULTS=$((SEGFAULTS + 1))
                FAILED=$((FAILED + 1))
                ;;
            *)
                echo -e "${RED}✗${NC} $BASENAME"
                FAILED=$((FAILED + 1))
                ;;
        esac
    else
        echo -e "${RED}?${NC} $BASENAME (no result)"
        FAILED=$((FAILED + 1))
    fi
done

echo ""
echo "=========================================="
echo "Summary"
echo "=========================================="
echo "Total:     $TOTAL"
if [ $SKIPPED -gt 0 ]; then
    echo -e "Skipped:   ${BLUE}$SKIPPED${NC}"
    RAN=$((TOTAL - SKIPPED))
    echo "Ran:       $RAN"
fi
echo -e "Passed:    ${GREEN}$PASSED${NC}"
echo -e "Warnings:  ${YELLOW}$WARNINGS${NC}"
echo -e "Failed:    ${RED}$FAILED${NC}"
if [ $SEGFAULTS -gt 0 ]; then
    echo -e "Segfaults: ${RED}$SEGFAULTS${NC}"
fi
if [ $TIMEOUTS -gt 0 ]; then
    echo -e "Timeouts:  ${RED}$TIMEOUTS${NC}"
fi
echo "=========================================="
echo ""
echo "Results in: $RESULTS_DIR/"
echo "Output files: $RESULTS_DIR/<testname>/<testname>.out"

# Show segfault cases for debugging
if [ $SEGFAULTS -gt 0 ]; then
    echo ""
    echo "Tests with segmentation faults:"
    for INP_FILE in "${INP_FILES[@]}"; do
        BASENAME=$(basename "$INP_FILE" .inp)
        if [ -f "$RESULTS_DIR/$BASENAME/status.txt" ]; then
            if grep -q "SEGFAULT" "$RESULTS_DIR/$BASENAME/status.txt"; then
                echo "  - $BASENAME (check $RESULTS_DIR/$BASENAME/run.log)"
            fi
        fi
    done
fi

# Copy successful outputs to main scratch directory
if [ $PASSED -gt 0 ]; then
    echo ""
    echo "Copying successful outputs to scratch directory..."
    for INP_FILE in "${INP_FILES[@]}"; do
        BASENAME=$(basename "$INP_FILE" .inp)
        if [ -f "$RESULTS_DIR/$BASENAME/status.txt" ]; then
            STATUS=$(cat "$RESULTS_DIR/$BASENAME/status.txt")
            if [ "$STATUS" = "PASS" ] || [ "$STATUS" = "WARNING" ]; then
                cp "$RESULTS_DIR/$BASENAME/${BASENAME}".* . 2>/dev/null || true
            fi
        fi
    done
    echo "Done"
fi

# Exit code
if [ $FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi
