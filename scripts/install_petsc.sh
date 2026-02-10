#!/bin/bash
# ==============================================================================
# PETSc Installation Script for NASTRAN-95
# ==============================================================================
# Automated installation of PETSc with all solver backends
#
# Usage:
#   ./install_petsc.sh [minimal|full|mkl]
#
# Options:
#   minimal  - Quick install with MUMPS only (5-10 min)
#   full     - All solvers: MUMPS, SuperLU, SuiteSparse, PaStiX (20-30 min)
#   mkl      - Full + Intel MKL PARDISO (requires MKL installed)
#
# Default: full
# ==============================================================================

set -e  # Exit on error

# Configuration
CONFIG=${1:-full}
INSTALL_DIR=${PETSC_DIR:-$HOME/petsc}
ARCH_NAME="linux-gnu-opt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# ==============================================================================
# Check System Requirements
# ==============================================================================

log_info "Checking system requirements..."

# Check OS
if [ ! -f /etc/os-release ]; then
    log_error "Cannot detect OS. This script is for Ubuntu/Debian."
    exit 1
fi

. /etc/os-release
if [[ "$ID" != "ubuntu" && "$ID" != "debian" && "$ID" != "pop" ]]; then
    log_warn "This script is designed for Ubuntu/Debian. Your OS: $ID"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Check if running as root (not recommended)
if [ "$EUID" -eq 0 ]; then
    log_warn "Running as root is not recommended"
    read -p "Continue? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# ==============================================================================
# Install Dependencies
# ==============================================================================

log_info "Installing build dependencies..."

# Check if sudo is available
if command -v sudo &> /dev/null; then
    SUDO="sudo"
else
    SUDO=""
    log_warn "sudo not found - assuming you have permissions"
fi

# Update package list
$SUDO apt-get update

# Install dependencies
$SUDO apt-get install -y \
    build-essential \
    gfortran \
    g++ \
    gcc \
    cmake \
    git \
    python3 \
    python3-distutils \
    libopenmpi-dev \
    openmpi-bin \
    libblas-dev \
    liblapack-dev \
    pkg-config \
    wget \
    curl \
    ca-certificates \
    || {
        log_error "Failed to install dependencies"
        exit 1
    }

log_success "Dependencies installed"

# ==============================================================================
# Download PETSc
# ==============================================================================

if [ -d "$INSTALL_DIR" ]; then
    log_warn "PETSc directory already exists: $INSTALL_DIR"
    read -p "Remove and reinstall? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        log_info "Removing old installation..."
        rm -rf "$INSTALL_DIR"
    else
        log_info "Using existing directory"
        cd "$INSTALL_DIR"
    fi
else
    log_info "Downloading PETSc..."
    git clone -b release https://gitlab.com/petsc/petsc.git "$INSTALL_DIR" || {
        log_error "Failed to clone PETSc"
        exit 1
    }
    cd "$INSTALL_DIR"
fi

log_success "PETSc source ready"

# ==============================================================================
# Configure PETSc
# ==============================================================================

log_info "Configuring PETSc (configuration: $CONFIG)..."

# Common options
COMMON_OPTS=(
    --with-cc=gcc
    --with-fc=gfortran
    --with-cxx=g++
    --with-debugging=0
    --with-scalar-type=real
    --with-precision=double
    --with-shared-libraries=1
)

# Configuration-specific options
case $CONFIG in
    minimal)
        log_info "Minimal configuration (MUMPS only)"
        ARCH_NAME="linux-gnu-min"
        CONFIG_OPTS=(
            "${COMMON_OPTS[@]}"
            --with-mpi=1
            --with-blaslapack-dir=/usr
            --download-mumps
            --download-scalapack
            --download-metis
            PETSC_ARCH=$ARCH_NAME
        )
        ;;

    full)
        log_info "Full configuration (all open-source solvers)"
        ARCH_NAME="linux-gnu-opt"
        CONFIG_OPTS=(
            "${COMMON_OPTS[@]}"
            --with-mpi=1
            --with-blaslapack-dir=/usr
            --download-mumps
            --download-scalapack
            --download-metis
            --download-parmetis
            --download-ptscotch
            --download-superlu
            --download-superlu_dist
            --download-suitesparse
            --download-pastix
            --download-hwloc
            PETSC_ARCH=$ARCH_NAME
        )
        ;;

    mkl)
        log_info "Full configuration + Intel MKL PARDISO"

        # Check if MKL is available
        if [ -z "$MKLROOT" ]; then
            log_warn "MKLROOT not set. Trying to find Intel MKL..."

            # Try common MKL locations
            MKL_PATHS=(
                "/opt/intel/oneapi/mkl/latest"
                "/opt/intel/mkl"
                "/usr"
            )

            for path in "${MKL_PATHS[@]}"; do
                if [ -d "$path/lib" ]; then
                    export MKLROOT="$path"
                    log_info "Found MKL at: $MKLROOT"
                    break
                fi
            done

            if [ -z "$MKLROOT" ]; then
                log_error "Intel MKL not found. Install with:"
                echo "  wget https://registrationcenter-download.intel.com/akdlm/IRC_NAS/[...]/intel-oneapi-base-toolkit-*.sh"
                echo "  sudo sh intel-oneapi-base-toolkit-*.sh"
                echo "  source /opt/intel/oneapi/setvars.sh"
                exit 1
            fi
        fi

        log_success "Using MKL from: $MKLROOT"

        ARCH_NAME="linux-gnu-mkl"
        CONFIG_OPTS=(
            "${COMMON_OPTS[@]}"
            --with-mpi=1
            --with-mkl_pardiso=1
            --with-mkl_pardiso-dir=$MKLROOT
            --with-blaslapack-dir=$MKLROOT
            --download-mumps
            --download-scalapack
            --download-metis
            --download-parmetis
            --download-ptscotch
            --download-superlu
            --download-superlu_dist
            --download-suitesparse
            --download-pastix
            PETSC_ARCH=$ARCH_NAME
        )
        ;;

    *)
        log_error "Unknown configuration: $CONFIG"
        echo "Usage: $0 [minimal|full|mkl]"
        exit 1
        ;;
esac

# Run configure
log_info "Running configure (this may take a few minutes)..."
./configure "${CONFIG_OPTS[@]}" || {
    log_error "Configure failed"
    exit 1
}

log_success "Configuration complete"

# ==============================================================================
# Build PETSc
# ==============================================================================

log_info "Building PETSc (this takes 10-30 minutes)..."
log_info "Using $(nproc) cores for parallel build"

make PETSC_DIR=$INSTALL_DIR PETSC_ARCH=$ARCH_NAME -j$(nproc) all || {
    log_error "Build failed"
    exit 1
}

log_success "Build complete"

# ==============================================================================
# Test Installation
# ==============================================================================

log_info "Running tests..."

make PETSC_DIR=$INSTALL_DIR PETSC_ARCH=$ARCH_NAME check || {
    log_error "Tests failed"
    exit 1
}

log_success "All tests passed"

# ==============================================================================
# Print Summary
# ==============================================================================

echo ""
echo "=========================================================================="
echo "  PETSc Installation Complete!"
echo "=========================================================================="
echo ""
echo "Installation directory: $INSTALL_DIR"
echo "Architecture:          $ARCH_NAME"
echo "Configuration:         $CONFIG"
echo ""
echo "To use PETSc, add these lines to your ~/.bashrc:"
echo ""
echo "  export PETSC_DIR=$INSTALL_DIR"
echo "  export PETSC_ARCH=$ARCH_NAME"
echo "  export PATH=\$PETSC_DIR/\$PETSC_ARCH/bin:\$PATH"
echo "  export LD_LIBRARY_PATH=\$PETSC_DIR/\$PETSC_ARCH/lib:\$LD_LIBRARY_PATH"
echo ""
echo "Or run:"
echo "  source $INSTALL_DIR/$ARCH_NAME/lib/petsc/conf/petscvariables"
echo ""

# Check which solvers are available
log_info "Checking available solvers..."

HAVE_MUMPS=$(grep -c "PETSC_HAVE_MUMPS = 1" $INSTALL_DIR/$ARCH_NAME/lib/petsc/conf/petscvariables || echo 0)
HAVE_SUPERLU=$(grep -c "PETSC_HAVE_SUPERLU = 1" $INSTALL_DIR/$ARCH_NAME/lib/petsc/conf/petscvariables || echo 0)
HAVE_UMFPACK=$(grep -c "PETSC_HAVE_UMFPACK = 1" $INSTALL_DIR/$ARCH_NAME/lib/petsc/conf/petscvariables || echo 0)
HAVE_PARDISO=$(grep -c "PETSC_HAVE_MKL_PARDISO = 1" $INSTALL_DIR/$ARCH_NAME/lib/petsc/conf/petscvariables || echo 0)
HAVE_PASTIX=$(grep -c "PETSC_HAVE_PASTIX = 1" $INSTALL_DIR/$ARCH_NAME/lib/petsc/conf/petscvariables || echo 0)

echo "Available solvers:"
[ $HAVE_MUMPS -eq 1 ] && echo "  ✓ MUMPS (parallel sparse direct)" || echo "  ✗ MUMPS"
[ $HAVE_SUPERLU -eq 1 ] && echo "  ✓ SuperLU (sparse LU)" || echo "  ✗ SuperLU"
[ $HAVE_UMFPACK -eq 1 ] && echo "  ✓ UMFPACK (SuiteSparse)" || echo "  ✗ UMFPACK"
[ $HAVE_PARDISO -eq 1 ] && echo "  ✓ MKL PARDISO (Intel)" || echo "  ✗ MKL PARDISO"
[ $HAVE_PASTIX -eq 1 ] && echo "  ✓ PaStiX (parallel)" || echo "  ✗ PaStiX"

echo ""
echo "=========================================================================="
echo "  Next Steps for NASTRAN-95 Integration"
echo "=========================================================================="
echo ""
echo "1. Set environment variables (add to ~/.bashrc)"
echo "2. Configure NASTRAN-95 with PETSc:"
echo "     cd /mnt/developer/git/aecs4u.it/NASTRAN-95/build"
echo "     cmake .. -DUSE_PETSC=ON"
echo "     make -j\$(nproc)"
echo ""
echo "3. Test NASTRAN with different solvers:"
echo "     ./nastran -pc_type lu -pc_factor_mat_solver_type mumps input.nas"
echo "     ./nastran -pc_type lu -pc_factor_mat_solver_type superlu input.nas"
echo ""
echo "For more information, see:"
echo "  docs/PETSC_INSTALLATION_GUIDE.md"
echo "  docs/EXISTING_SOLVER_ABSTRACTION_FRAMEWORKS.md"
echo ""
echo "=========================================================================="

exit 0
