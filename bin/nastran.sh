#!/bin/bash
# NASTRAN-95 Wrapper Script
# Automatically sets output filenames based on input file rootname

set -e

# Check if input file is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <input_file.inp>"
    echo ""
    echo "Example: $0 d01000a.inp"
    echo "         Creates: d01000a.log, d01000a.dic, d01000a.pun, etc."
    exit 1
fi

INPUT_FILE="$1"

# Check if input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: Input file '$INPUT_FILE' not found"
    exit 1
fi

# Get the directory and basename
INPUT_DIR=$(dirname "$INPUT_FILE")
INPUT_BASE=$(basename "$INPUT_FILE" .inp)
INPUT_BASE=$(basename "$INPUT_BASE" .INP)

# Set output directory (same as input file location)
OUTPUT_DIR="$INPUT_DIR"
cd "$OUTPUT_DIR"

# Find NASTRAN executable
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/../build/bin/nastran" ]; then
    NASTRAN_EXE="$SCRIPT_DIR/../build/bin/nastran"
elif [ -f "$SCRIPT_DIR/nastran" ]; then
    NASTRAN_EXE="$SCRIPT_DIR/nastran"
else
    echo "Error: NASTRAN executable not found"
    exit 1
fi

# Add Rust parser bridge library to LD_LIBRARY_PATH
RUST_BRIDGE_DIR="$SCRIPT_DIR/../nastran_parser_bridge/target/release"
if [ -f "$RUST_BRIDGE_DIR/libnastran_parser_bridge.so" ]; then
    export LD_LIBRARY_PATH="$RUST_BRIDGE_DIR:$LD_LIBRARY_PATH"
fi

# Set up environment variables with rootname-based filenames
export DBMEM=12000000
export OCMEM=2000000
export RFDIR="${SCRIPT_DIR}/../rf"
export DIRCTY="/tmp"
export INPFILE="$(realpath "$INPUT_FILE")"
export LOGNM="${INPUT_BASE}.log"
export DICTNM="${INPUT_BASE}.dic"
export PLTNM="${INPUT_BASE}.plt"
export PUNCHNM="${INPUT_BASE}.pun"
export NPTPNM="${INPUT_BASE}.nptp"
export OPTPNM="none"

# Set Fortran units to none (not used)
for i in {11..21}; do
    export FTN${i}=none
done

# Set SOF files to none
for i in {1..10}; do
    export SOF${i}=none
done

# Check if RFDIR path is too long (NASTRAN limitation: max ~40 chars)
RFDIR_LEN=${#RFDIR}
if [ $RFDIR_LEN -gt 40 ]; then
    # Create short symlink in /tmp
    RFDIR_SHORT="/tmp/nrf"
    if [ ! -L "$RFDIR_SHORT" ]; then
        ln -sf "$RFDIR" "$RFDIR_SHORT"
    fi
    export RFDIR="$RFDIR_SHORT"
fi

echo "========================================"
echo "NASTRAN-95 Execution"
echo "========================================"
echo "Input:  $(basename $INPUT_FILE)"
echo "Output: ${INPUT_BASE}.log"
echo "        ${INPUT_BASE}.dic"
echo "        ${INPUT_BASE}.pun"
echo "        ${INPUT_BASE}.plt"
echo "========================================"
echo ""

# Run NASTRAN with input redirection
"$NASTRAN_EXE" < "$INPUT_FILE" > "${INPUT_BASE}.out" 2>&1

EXIT_CODE=$?

echo ""
echo "========================================"
echo "Execution complete (exit code: $EXIT_CODE)"
echo "========================================"
echo "Output files created:"
ls -lh "${INPUT_BASE}".{out,log,dic,pun,plt,nptp} 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
echo ""

if [ $EXIT_CODE -ne 0 ]; then
    echo "Errors occurred. Check ${INPUT_BASE}.out for details."
fi

exit $EXIT_CODE
