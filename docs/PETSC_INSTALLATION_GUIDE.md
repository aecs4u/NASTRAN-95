# PETSc Installation Guide for Ubuntu (NASTRAN-95)
**Date:** 2026-02-09
**Target:** Ubuntu 22.04+ (also works on Debian, Pop!_OS, etc.)
**Goal:** Install PETSc with all solver backends for NASTRAN-95

---

## Quick Start (TL;DR)

```bash
# Install dependencies
sudo apt install -y build-essential gfortran cmake git python3 \
                    libopenmpi-dev openmpi-bin libblas-dev liblapack-dev

# Download PETSc
git clone -b release https://gitlab.com/petsc/petsc.git ~/petsc
cd ~/petsc

# Configure with all solvers (recommended for NASTRAN-95)
./configure \
  --with-cc=gcc --with-fc=gfortran --with-cxx=g++ \
  --with-debugging=0 --with-scalar-type=real --with-precision=double \
  --with-blas-lapack-dir=/usr \
  --download-mumps \
  --download-scalapack \
  --download-metis \
  --download-parmetis \
  --download-superlu \
  --download-superlu_dist \
  --download-suitesparse \
  --download-pastix \
  PETSC_ARCH=linux-gnu-opt

# Build (takes 10-30 minutes)
make all

# Test
make check

# Set environment variables (add to ~/.bashrc)
export PETSC_DIR=$HOME/petsc
export PETSC_ARCH=linux-gnu-opt
```

---

## Table of Contents

1. [Method 1: System Packages (Quick)](#method-1-system-packages-quick)
2. [Method 2: Build from Source (Recommended)](#method-2-build-from-source-recommended)
3. [Method 3: Spack (HPC Alternative)](#method-3-spack-hpc-alternative)
4. [Verification](#verification)
5. [Troubleshooting](#troubleshooting)
6. [Integration with NASTRAN-95](#integration-with-nastran-95)

---

## Method 1: System Packages (Quick)

**Pros:** Fast, easy, no compilation
**Cons:** Limited solver backends, may be older version
**Use Case:** Quick testing, CI/CD

### Step 1: Install PETSc from Ubuntu Repos

```bash
# Install PETSc and common solvers
sudo apt update
sudo apt install -y \
  petsc-dev \
  libpetsc-real-dev \
  libmumps-dev \
  libsuperlu-dev \
  libsuitesparse-dev \
  libscalapack-mpi-dev

# Check version
pkg-config --modversion PETSc
```

### Step 2: Verify Installation

```bash
# PETSc should be in /usr
ls /usr/lib/x86_64-linux-gnu/libpetsc*
ls /usr/include/petsc*
```

### Step 3: Set Environment (Optional)

```bash
# Add to ~/.bashrc
export PETSC_DIR=/usr
export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:$PKG_CONFIG_PATH
```

### Limitations of System Packages

❌ **No MKL PARDISO** (requires Intel MKL)
❌ **No PaStiX** (usually not packaged)
❌ **Older version** (Ubuntu 22.04 has PETSc 3.16, current is 3.24)
✅ **Has MUMPS, SuperLU, UMFPACK** (basic solvers work)

**Verdict:** Good for quick testing, but **use Method 2 for production**.

---

## Method 2: Build from Source (Recommended)

**Pros:** Latest version, all solvers, optimized build
**Cons:** Takes 10-30 minutes to compile
**Use Case:** Production NASTRAN-95 deployment

### Step 1: Install Build Dependencies

```bash
sudo apt update
sudo apt install -y \
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
  curl

# Optional: Install Intel MKL for PARDISO (see MKL section below)
```

### Step 2: Download PETSc

```bash
# Clone PETSc repository
cd ~
git clone -b release https://gitlab.com/petsc/petsc.git
cd petsc

# Check out latest release (optional)
git checkout v3.24.0  # Or latest version
```

### Step 3: Configure PETSc with All Solvers

Choose one of these configurations:

#### Option A: Full Configuration (All Solvers - Recommended)

```bash
./configure \
  --with-cc=gcc \
  --with-fc=gfortran \
  --with-cxx=g++ \
  --with-debugging=0 \
  --with-scalar-type=real \
  --with-precision=double \
  --with-shared-libraries=1 \
  --with-mpi=1 \
  --with-blas-lapack-dir=/usr \
  --download-mumps \
  --download-scalapack \
  --download-metis \
  --download-parmetis \
  --download-ptscotch \
  --download-superlu \
  --download-superlu_dist \
  --download-suitesparse \
  --download-pastix \
  --download-hwloc \
  PETSC_ARCH=linux-gnu-opt

# This downloads and builds:
# - MUMPS (parallel sparse direct)
# - ScaLAPACK (parallel linear algebra)
# - METIS/ParMETIS (graph partitioning)
# - PT-SCOTCH (reordering)
# - SuperLU (sequential sparse LU)
# - SuperLU_DIST (parallel sparse LU)
# - SuiteSparse (UMFPACK, CHOLMOD, KLU, etc.)
# - PaStiX (parallel sparse direct)
# - HWLOC (hardware locality)
```

#### Option B: Minimal Configuration (Fast Build)

```bash
./configure \
  --with-cc=gcc --with-fc=gfortran --with-cxx=g++ \
  --with-debugging=0 \
  --download-mumps \
  --download-scalapack \
  PETSC_ARCH=linux-gnu-min

# Only builds MUMPS (good for quick testing)
```

#### Option C: With Intel MKL (PARDISO + Optimized BLAS)

```bash
# First install Intel MKL (see below)
source /opt/intel/oneapi/setvars.sh

./configure \
  --with-cc=gcc --with-fc=gfortran --with-cxx=g++ \
  --with-debugging=0 \
  --with-mkl_pardiso=1 \
  --with-mkl_pardiso-dir=$MKLROOT \
  --with-blas-lapack-dir=$MKLROOT \
  --download-mumps \
  --download-scalapack \
  --download-metis \
  --download-parmetis \
  --download-superlu \
  --download-superlu_dist \
  --download-suitesparse \
  PETSC_ARCH=linux-gnu-mkl

# This uses Intel MKL for:
# - BLAS/LAPACK (optimized)
# - MKL PARDISO (direct solver)
```

### Step 4: Build PETSc

```bash
# The configure script prints the make command at the end
# Typically:
make PETSC_DIR=$HOME/petsc PETSC_ARCH=linux-gnu-opt all

# Or follow the exact command printed by configure

# Build time: 10-30 minutes depending on configuration
# Use -j to parallelize: make -j$(nproc) all
```

**What's happening:**
- Downloads all solver packages (MUMPS, SuperLU, etc.)
- Compiles each package with correct flags
- Builds PETSc with all solvers linked

### Step 5: Test Installation

```bash
make PETSC_DIR=$HOME/petsc PETSC_ARCH=linux-gnu-opt check

# Run comprehensive tests (optional)
make PETSC_DIR=$HOME/petsc PETSC_ARCH=linux-gnu-opt test
```

**Expected output:**
```
Running check examples to verify correct installation
...
Completed test examples
```

### Step 6: Set Environment Variables

Add to `~/.bashrc` (or `~/.zshrc`):

```bash
# PETSc environment
export PETSC_DIR=$HOME/petsc
export PETSC_ARCH=linux-gnu-opt
export PATH=$PETSC_DIR/$PETSC_ARCH/bin:$PATH
export LD_LIBRARY_PATH=$PETSC_DIR/$PETSC_ARCH/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$PETSC_DIR/$PETSC_ARCH/lib/pkgconfig:$PKG_CONFIG_PATH
```

Apply changes:
```bash
source ~/.bashrc
```

### Step 7: Verify Solver Backends

```bash
# Check which solvers are available
$PETSC_DIR/$PETSC_ARCH/bin/petscmpiexec -help | grep -i solver

# Or check configuration
cat $PETSC_DIR/$PETSC_ARCH/lib/petsc/conf/petscvariables | grep -i mumps
cat $PETSC_DIR/$PETSC_ARCH/lib/petsc/conf/petscvariables | grep -i superlu
cat $PETSC_DIR/$PETSC_ARCH/lib/petsc/conf/petscvariables | grep -i umfpack
```

---

## Method 3: Spack (HPC Alternative)

**Pros:** Handles all dependencies automatically, reproducible builds
**Cons:** Spack learning curve, slower initial install
**Use Case:** HPC clusters, reproducible environments

### Step 1: Install Spack

```bash
# Clone Spack
git clone -c feature.manyFiles=true https://github.com/spack/spack.git ~/spack
cd ~/spack

# Add to ~/.bashrc
export SPACK_ROOT=$HOME/spack
source $SPACK_ROOT/share/spack/setup-env.sh
```

### Step 2: Install PETSc with Spack

```bash
# Full configuration with all solvers
spack install petsc+mumps+superlu-dist+suitesparse+pastix+metis+parmetis

# With MKL PARDISO
spack install petsc+mumps+superlu-dist+suitesparse+mkl_pardiso

# List available variants
spack info petsc
```

### Step 3: Load PETSc

```bash
# Load PETSc environment
spack load petsc

# Check
spack find -v petsc
```

---

## Intel MKL Installation (For PARDISO)

### Method A: Intel oneAPI (Free, Recommended)

```bash
# Download oneAPI installer
wget https://registrationcenter-download.intel.com/akdlm/IRC_NAS/72d41c4e-4b0f-4d5e-8ec0-d8e0f9c9f2dc/intel-oneapi-base-toolkit-2024.1.0.596_offline.sh

# Install (GUI or silent)
sudo sh intel-oneapi-base-toolkit-*.sh

# Or silent install
sudo sh intel-oneapi-base-toolkit-*.sh -a --silent --eula accept

# Load MKL
source /opt/intel/oneapi/setvars.sh

# Add to ~/.bashrc
echo "source /opt/intel/oneapi/setvars.sh" >> ~/.bashrc
```

### Method B: MKL from Ubuntu Repos (Older)

```bash
sudo apt install intel-mkl-full
export MKLROOT=/usr
```

### Verify MKL

```bash
echo $MKLROOT
ls $MKLROOT/lib/libmkl*
```

---

## Verification

### Test 1: Basic PETSc Test

```bash
# Create test program
cat > test_petsc.f90 << 'EOF'
program test_petsc
  implicit none
#include <petsc/finclude/petscsys.h>
  PetscErrorCode :: ierr

  call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
  if (ierr /= 0) then
    print *, 'ERROR: PetscInitialize failed'
    stop 1
  end if

  print *, 'PETSc initialized successfully!'

  call PetscFinalize(ierr)
end program test_petsc
EOF

# Compile
mpif90 -I$PETSC_DIR/include -I$PETSC_DIR/$PETSC_ARCH/include \
       test_petsc.f90 \
       -L$PETSC_DIR/$PETSC_ARCH/lib -lpetsc \
       -o test_petsc

# Run
./test_petsc
```

**Expected output:**
```
PETSc initialized successfully!
```

### Test 2: Solver Backend Test

```bash
# Test MUMPS
mpirun -n 1 $PETSC_DIR/src/ksp/ksp/tutorials/ex2 \
  -pc_type lu -pc_factor_mat_solver_type mumps -ksp_view

# Test SuperLU
mpirun -n 1 $PETSC_DIR/src/ksp/ksp/tutorials/ex2 \
  -pc_type lu -pc_factor_mat_solver_type superlu

# Test UMFPACK (from SuiteSparse)
mpirun -n 1 $PETSC_DIR/src/ksp/ksp/tutorials/ex2 \
  -pc_type lu -pc_factor_mat_solver_type umfpack

# Test MKL PARDISO (if MKL installed)
mpirun -n 1 $PETSC_DIR/src/ksp/ksp/tutorials/ex2 \
  -pc_type lu -pc_factor_mat_solver_type mkl_pardiso
```

### Test 3: List Available Solvers

```bash
# Run any PETSc program with -help
cd $PETSC_DIR/src/ksp/ksp/tutorials
make ex2
./ex2 -help | grep -A 20 "mat_solver_type"
```

**Example output:**
```
MatSolverType: mumps superlu superlu_dist umfpack cholmod ...
```

---

## Troubleshooting

### Issue 1: Configure fails with "Cannot download package"

**Solution:**
```bash
# Use wget instead of curl
./configure --with-download-wget ...

# Or specify mirrors
./configure --with-packages-download-dir=/tmp/petsc-downloads ...
```

### Issue 2: "libmpi.so not found"

**Solution:**
```bash
# Install OpenMPI
sudo apt install libopenmpi-dev openmpi-bin

# Or add to LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/openmpi/lib:$LD_LIBRARY_PATH
```

### Issue 3: FORTRAN module files not found

**Solution:**
```bash
# PETSc FORTRAN modules are in:
# $PETSC_DIR/$PETSC_ARCH/include

# For gfortran, add:
-I$PETSC_DIR/include -I$PETSC_DIR/$PETSC_ARCH/include
```

### Issue 4: "Cannot find MUMPS/PARDISO at runtime"

**Solution:**
```bash
# Check if solver was built
cat $PETSC_DIR/$PETSC_ARCH/lib/petsc/conf/petscvariables | grep HAVE_MUMPS

# Should see: PETSC_HAVE_MUMPS = 1

# If not, reconfigure PETSc with --download-mumps
```

### Issue 5: MKL PARDISO not found

**Solution:**
```bash
# Load MKL environment
source /opt/intel/oneapi/setvars.sh

# Reconfigure PETSc with MKL
./configure --with-mkl_pardiso=1 --with-mkl_pardiso-dir=$MKLROOT ...
```

### Issue 6: Build is very slow

**Solution:**
```bash
# Use parallel make
make -j$(nproc) all

# Or specify cores
make -j8 all
```

---

## Integration with NASTRAN-95 CMake

### Add to NASTRAN-95 CMakeLists.txt

```cmake
# Find PETSc
option(USE_PETSC "Enable PETSc solver abstraction" ON)

if(USE_PETSC)
  # Method 1: pkg-config (if available)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(PETSC REQUIRED PETSc)

  # Method 2: Manual paths
  if(NOT PETSC_FOUND)
    set(PETSC_DIR $ENV{PETSC_DIR})
    set(PETSC_ARCH $ENV{PETSC_ARCH})

    if(EXISTS ${PETSC_DIR}/${PETSC_ARCH}/lib/pkgconfig/PETSc.pc)
      set(PKG_CONFIG_PATH ${PETSC_DIR}/${PETSC_ARCH}/lib/pkgconfig)
      pkg_check_modules(PETSC REQUIRED PETSc)
    else()
      message(FATAL_ERROR "PETSc not found. Set PETSC_DIR and PETSC_ARCH.")
    endif()
  endif()

  message(STATUS "PETSc found:")
  message(STATUS "  Version: ${PETSC_VERSION}")
  message(STATUS "  Include: ${PETSC_INCLUDE_DIRS}")
  message(STATUS "  Libs:    ${PETSC_LIBRARIES}")

  # Add PETSc to nastran
  target_include_directories(nastran PRIVATE ${PETSC_INCLUDE_DIRS})
  target_link_libraries(nastran PRIVATE ${PETSC_LIBRARIES})
  target_compile_definitions(nastran PRIVATE -DHAVE_PETSC=1)
endif()
```

### Build NASTRAN-95 with PETSc

```bash
cd /mnt/developer/git/aecs4u.it/NASTRAN-95
mkdir -p build && cd build

cmake .. \
  -DUSE_PETSC=ON \
  -DCMAKE_BUILD_TYPE=Release

make -j$(nproc)
```

---

## Configuration Summary

### Recommended Configuration for NASTRAN-95

```bash
cd ~/petsc

./configure \
  --with-cc=gcc \
  --with-fc=gfortran \
  --with-cxx=g++ \
  --with-debugging=0 \
  --with-scalar-type=real \
  --with-precision=double \
  --with-shared-libraries=1 \
  --with-mpi=1 \
  --with-blaslapack-dir=/usr \
  --download-mumps \
  --download-scalapack \
  --download-metis \
  --download-parmetis \
  --download-ptscotch \
  --download-superlu \
  --download-superlu_dist \
  --download-suitesparse \
  --download-pastix \
  PETSC_ARCH=linux-gnu-opt

make all
make check
```

**This provides:**
- ✅ MUMPS (primary solver for NASTRAN)
- ✅ SuperLU + SuperLU_DIST (alternative sparse direct)
- ✅ UMFPACK/CHOLMOD/KLU (from SuiteSparse)
- ✅ PaStiX (high-performance parallel)
- ✅ METIS/ParMETIS/PT-SCOTCH (ordering/partitioning)
- ✅ ScaLAPACK (parallel linear algebra)
- ✅ MPI support (for future parallel NASTRAN)

**Add MKL PARDISO later:**
```bash
# Install Intel oneAPI MKL
source /opt/intel/oneapi/setvars.sh

# Reconfigure PETSc
./configure \
  --with-mkl_pardiso=1 \
  --with-mkl_pardiso-dir=$MKLROOT \
  --with-blaslapack-dir=$MKLROOT \
  [... other options ...]
  PETSC_ARCH=linux-gnu-mkl

make all
```

---

## Quick Reference

### Environment Variables
```bash
export PETSC_DIR=$HOME/petsc
export PETSC_ARCH=linux-gnu-opt
export PATH=$PETSC_DIR/$PETSC_ARCH/bin:$PATH
export LD_LIBRARY_PATH=$PETSC_DIR/$PETSC_ARCH/lib:$LD_LIBRARY_PATH
```

### Compiler Flags (FORTRAN)
```bash
# Compile
-I$PETSC_DIR/include -I$PETSC_DIR/$PETSC_ARCH/include

# Link
-L$PETSC_DIR/$PETSC_ARCH/lib -lpetsc
```

### Runtime Solver Selection
```bash
# MUMPS
-pc_type lu -pc_factor_mat_solver_type mumps

# PARDISO
-pc_type lu -pc_factor_mat_solver_type mkl_pardiso

# SuperLU
-pc_type lu -pc_factor_mat_solver_type superlu

# UMFPACK
-pc_type lu -pc_factor_mat_solver_type umfpack

# Iterative GMRES + ILU
-ksp_type gmres -pc_type ilu
```

---

## Next Steps

After installing PETSc:

1. **Verify installation**: Run test programs above
2. **Check available solvers**: `./ex2 -help | grep mat_solver_type`
3. **Integrate with NASTRAN-95**: Add CMake FindPETSc
4. **Create interface layer**: `src/core/solvers/nastran_petsc_interface.f90`
5. **Test with NASTRAN**: Run simple static analysis with PETSc

---

## Additional Resources

- **PETSc Documentation**: https://petsc.org/release/docs/
- **Installation Guide**: https://petsc.org/release/install/
- **FORTRAN Interface**: https://petsc.org/release/manual/fortran/
- **Mailing List**: petsc-users@mcs.anl.gov
- **Tutorials**: https://petsc.org/release/src/ksp/ksp/tutorials/

---

**Installation time:** 10-30 minutes (depending on configuration)
**Disk space:** ~2-5 GB (with all solvers)
**Recommended for NASTRAN-95:** Method 2 (Build from Source) with full configuration
