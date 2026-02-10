#!/bin/bash
# Test Rust bridge integration

set -x  # Enable debug output

# Clean up previous run
rm -f /tmp/rust_debug.txt
rm -f /tmp/nastran_test.log

# Set environment
export INPFILE="$(pwd)/examples/input/d01001a.inp"
export DBMEM=12000000
export OCMEM=2000000
export RFDIR=/tmp/nrf
export DIRCTY=/tmp
export LOGNM=/tmp/d01001a.log
export DICTNM=/tmp/d01001a.dic
export PLTNM=/tmp/d01001a.plt
export PUNCHNM=/tmp/d01001a.pun
export NPTPNM=/tmp/d01001a.nptp
export OPTPNM=none

# Set Fortran units to none
for i in {11..21}; do
    export FTN${i}=none
done

# Set SOF files to none
for i in {1..10}; do
    export SOF${i}=none
done

export LD_LIBRARY_PATH="$(pwd)/nastran_parser_bridge/target/release:$LD_LIBRARY_PATH"

echo "INPFILE=$INPFILE"
echo "Executable: $(pwd)/build/bin/nastran"
echo "Input file exists: $(test -f "$INPFILE" && echo YES || echo NO)"
echo "Rust bridge lib exists: $(test -f nastran_parser_bridge/target/release/libnastran_parser_bridge.so && echo YES || echo NO)"

# Run NASTRAN with timeout
NASTRAN_EXE="$(pwd)/build/bin/nastran"
cd examples/input
timeout 10 "$NASTRAN_EXE" d01001a.inp < d01001a.inp > /tmp/nastran_test.log 2>&1
EXIT_CODE=$?

echo "Exit code: $EXIT_CODE"
echo ""
echo "=== Debug file status ==="
if [ -f /tmp/rust_debug.txt ]; then
    echo "✓ Debug file created"
    cat /tmp/rust_debug.txt
else
    echo "✗ Debug file NOT created"
fi

echo ""
echo "=== NASTRAN output (first 100 lines) ==="
head -100 /tmp/nastran_test.log
