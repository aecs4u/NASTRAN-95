# MUMPS Backend Integration (CMake + Solver Skeleton)

## Purpose

This note defines the initial wiring for an optional MUMPS backend in the
modernized solver stack.

Current implementation state:

- CMake options and dependency hooks are in place.
- `linalg_backend_mumps_module.f90` provides a stable API surface.
- DMUMPS workflow is wired for `JOB=-1` (init), `JOB=4` (analyze/factor),
  `JOB=3` (solve), and `JOB=-2` (finalize).
- CSR input is converted to COO triplets (`IRN/JCN/A`) before factorization.

## CMake Options

| Option | Type | Default | Meaning |
|---|---|---|---|
| `USE_MUMPS` | BOOL | `OFF` | Enable MUMPS backend integration |
| `MUMPS_USE_MPI` | BOOL | `ON` | Link `MPI::MPI_Fortran` with backend |
| `MUMPS_ROOT` | PATH | empty | Root of MUMPS installation (optional hint) |
| `MUMPS_INCLUDE_DIR` | PATH | empty | Directory containing `dmumps_struc.h` |
| `MUMPS_LIBRARIES` | STRING | empty | Semicolon-separated MUMPS/scalapack link list |

## Typical Configure Commands

### 1) Auto-discovery with `MUMPS_ROOT`

```bash
cmake -S . -B build \
  -DUSE_MUMPS=ON \
  -DMUMPS_ROOT=/opt/mumps-5.8.2 \
  -DMUMPS_USE_MPI=ON \
  -DUSE_EXTERNAL_BLAS=ON \
  -DBUILD_EXAMPLES=OFF \
  -DBUILD_TESTS=OFF
```

### 2) Explicit include and libraries

```bash
cmake -S . -B build \
  -DUSE_MUMPS=ON \
  -DMUMPS_USE_MPI=ON \
  -DMUMPS_INCLUDE_DIR=/opt/mumps/include \
  -DMUMPS_LIBRARIES="/opt/mumps/lib/libdmumps.so;/opt/mumps/lib/libmumps_common.so;/opt/mumps/lib/libpord.so;/opt/scalapack/lib/libscalapack.so" \
  -DUSE_EXTERNAL_BLAS=ON \
  -DBUILD_EXAMPLES=OFF \
  -DBUILD_TESTS=OFF
```

## Backend API Skeleton

File: `src/solvers/backends/linalg_backend_mumps_module.f90`

Exposed entry points:

- `mumps_backend_available()`
- `mumps_initialize(...)`
- `mumps_factor_csr(...)`
- `mumps_solve_dense_rhs(...)`
- `mumps_finalize(...)`

Status codes:

- `MUMPS_STATUS_OK`
- `MUMPS_STATUS_NOT_BUILT`
- `MUMPS_STATUS_INVALID_ARGUMENT`
- `MUMPS_STATUS_NOT_FACTORIZED`
- `MUMPS_STATUS_INIT_FAILED`
- `MUMPS_STATUS_FACTOR_FAILED`
- `MUMPS_STATUS_SOLVE_FAILED`

## Next Technical Step

1. Add a tiny integration test that factors and solves a 3x3 sparse system
   and checks `x` against known values.
2. Add INFOG-based diagnostics table in logs (most common negative codes).
3. Support repeated solves with multiple RHS blocks in benchmark harness.
4. Add regression tests for solver parity against legacy path.
