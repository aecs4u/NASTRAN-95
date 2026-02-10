#!/bin/bash
# Build script for NASTRAN Rust Parser Bridge

set -e

echo "=========================================="
echo "Building NASTRAN Rust Parser Bridge"
echo "=========================================="

# Check if cargo is installed
if ! command -v cargo &> /dev/null; then
    echo "ERROR: Rust/cargo not found!"
    echo "Install from: https://rustup.rs/"
    exit 1
fi

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "ERROR: Python 3 not found!"
    exit 1
fi

# Check if pyNastran is installed
if ! python3 -c "import pyNastran" 2>/dev/null; then
    echo "WARNING: pyNastran not found"
    echo "Install with: pip install pyNastran"
    echo "Continuing anyway..."
fi

# Get Python info for PyO3
export PYTHON_SYS_EXECUTABLE=$(which python3)
PYTHON_VERSION=$(python3 --version | cut -d' ' -f2 | cut -d'.' -f1,2)
PYTHON_LIB_DIR=$(python3 -c "import sysconfig; print(sysconfig.get_config_var('LIBDIR'))")

echo ""
echo "Python Configuration:"
echo "  Executable: $PYTHON_SYS_EXECUTABLE"
echo "  Version: $PYTHON_VERSION"
echo "  Lib Dir: $PYTHON_LIB_DIR"
echo ""

# Build Rust library
echo "Building Rust library..."
cargo build --release

if [ $? -eq 0 ]; then
    echo ""
    echo "✓ Build successful!"
    echo ""
    echo "Library locations:"
    ls -lh target/release/*.{so,a} 2>/dev/null || ls -lh target/release/*.{dylib,a} 2>/dev/null
    echo ""
    echo "To use in NASTRAN-95:"
    echo "  1. Add to CMakeLists.txt:"
    echo "     link_directories(\${PROJECT_SOURCE_DIR}/nastran_parser_bridge/target/release)"
    echo "     target_link_libraries(nastran nastran_parser_bridge)"
    echo ""
    echo "  2. Rebuild NASTRAN:"
    echo "     cd build && cmake .. && cmake --build ."
    echo ""
else
    echo ""
    echo "✗ Build failed!"
    exit 1
fi
