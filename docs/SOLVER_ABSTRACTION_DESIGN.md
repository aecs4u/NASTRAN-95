# NASTRAN-95 Solver Abstraction Layer Design
**Date:** 2026-02-09
**Status:** Design Phase
**Goal:** Pluggable solver backend architecture for interchangeable solver libraries

---

## 1. Architecture Overview

### Design Principles

1. **Pluggable Backends**: Any solver library can be added as a backend
2. **Runtime Selection**: Choose solver at runtime via config file or API
3. **Compile-Time Options**: Enable/disable backends via CMake
4. **Zero Overhead**: No performance penalty when using native solvers
5. **Backward Compatible**: Existing code works without modification
6. **Type Safety**: FORTRAN 2003 abstract interfaces for compile-time checks

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Analysis Modules                          │
│            (Static, Modal, Dynamic, Frequency)               │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│              Solver Interface (Facade)                       │
│         src/core/solvers/solver_interface.f90               │
│  • solve_linear()    • solve_eigen()   • solve_complex()    │
│  • Backend selection • Config loading  • Error handling     │
└──────────────────────┬───────────────────────────────────────┘
                       │
        ┌──────────────┼──────────────┬──────────────┐
        ▼              ▼              ▼              ▼
┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
│   Native    │ │   PARDISO   │ │    MUMPS    │ │    PETSc    │
│   Backend   │ │   Backend   │ │   Backend   │ │   Backend   │
│  (FBS/DCOMP)│ │  (MKL/Panua)│ │ (Parallel)  │ │ (Iterative) │
└─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘
```

---

## 2. Module Structure

### Directory Layout

```
src/core/solvers/
├── solver_interface.f90          # Main interface (FORTRAN 2003)
├── solver_types.f90               # Common types and constants
├── solver_config.f90              # Configuration management
├── solver_registry.f90            # Backend registration
│
├── backends/
│   ├── backend_native.f90         # Native FBS/DCOMP wrapper
│   ├── backend_pardiso.f90        # MKL PARDISO adapter
│   ├── backend_mumps.f90          # MUMPS adapter
│   ├── backend_umfpack.f90        # UMFPACK adapter (C wrapper)
│   ├── backend_superlu.f90        # SuperLU adapter (C wrapper)
│   └── backend_petsc.f90          # PETSc KSP adapter
│
├── wrappers/
│   ├── umfpack_wrapper.c          # C wrapper for UMFPACK
│   ├── superlu_wrapper.c          # C wrapper for SuperLU
│   └── iso_c_utils.f90            # FORTRAN ISO_C_BINDING helpers
│
└── tests/
    ├── test_solver_interface.f90
    └── golden_matrices/           # Test matrices with known solutions
```

---

## 3. Core Interfaces

### 3.1 Abstract Solver Interface (FORTRAN 2003)

```fortran
! src/core/solvers/solver_types.f90
module solver_types
  use, intrinsic :: iso_fortran_env, only: real64, int32
  implicit none

  ! Solver backend identifiers
  integer, parameter :: SOLVER_NATIVE   = 1
  integer, parameter :: SOLVER_PARDISO  = 2
  integer, parameter :: SOLVER_MUMPS    = 3
  integer, parameter :: SOLVER_UMFPACK  = 4
  integer, parameter :: SOLVER_SUPERLU  = 5
  integer, parameter :: SOLVER_PETSC    = 6

  ! Matrix types
  integer, parameter :: MATRIX_GENERAL       = 1  ! Unsymmetric
  integer, parameter :: MATRIX_SYMMETRIC     = 2  ! Symmetric
  integer, parameter :: MATRIX_SPD           = 3  ! Symmetric positive definite
  integer, parameter :: MATRIX_COMPLEX       = 4  ! Complex
  integer, parameter :: MATRIX_COMPLEX_SPD   = 5  ! Complex Hermitian

  ! Matrix storage formats
  integer, parameter :: STORAGE_DENSE        = 1
  integer, parameter :: STORAGE_COO          = 2  ! Coordinate (triplet)
  integer, parameter :: STORAGE_CSR          = 3  ! Compressed Sparse Row
  integer, parameter :: STORAGE_CSC          = 4  ! Compressed Sparse Column

  ! Solver options
  type :: solver_options_t
    integer :: backend_id                ! Which solver to use
    integer :: matrix_type               ! Matrix properties
    integer :: storage_format            ! Sparse format
    logical :: use_parallel              ! Enable MPI/OpenMP
    integer :: num_threads               ! Thread count (0=auto)
    real(real64) :: tolerance            ! Iterative solver tolerance
    integer :: max_iterations            ! Max iterations (iterative)
    logical :: reuse_symbolic            ! Reuse factorization
    logical :: verbose                   ! Print diagnostics
  end type solver_options_t

  ! Solution statistics
  type :: solver_stats_t
    integer :: status                    ! 0=success, <0=error
    real(real64) :: time_symbolic        ! Symbolic factorization time
    real(real64) :: time_numeric         ! Numeric factorization time
    real(real64) :: time_solve           ! Solve time
    integer :: iterations                ! Iterations (if iterative)
    real(real64) :: residual_norm        ! Final residual
    integer(8) :: memory_used            ! Peak memory (bytes)
  end type solver_stats_t

end module solver_types
```

### 3.2 Solver Backend Interface

```fortran
! src/core/solvers/solver_interface.f90
module solver_interface
  use solver_types
  implicit none
  private
  public :: solver_backend_t, solver_init, solver_solve, solver_finalize

  ! Abstract base class for solver backends
  type, abstract :: solver_backend_t
    character(len=32) :: name
    integer :: backend_id
    logical :: supports_parallel
    logical :: supports_complex
    logical :: supports_iterative
  contains
    procedure(init_interface), deferred :: initialize
    procedure(solve_interface), deferred :: solve
    procedure(finalize_interface), deferred :: finalize
    procedure(supports_interface), deferred :: supports_matrix_type
  end type solver_backend_t

  ! Abstract interfaces
  abstract interface
    subroutine init_interface(this, opts, stat)
      import :: solver_backend_t, solver_options_t, solver_stats_t
      class(solver_backend_t), intent(inout) :: this
      type(solver_options_t), intent(in) :: opts
      type(solver_stats_t), intent(out) :: stat
    end subroutine init_interface

    subroutine solve_interface(this, n, a, ia, ja, b, x, opts, stat)
      import :: solver_backend_t, solver_options_t, solver_stats_t, real64
      class(solver_backend_t), intent(inout) :: this
      integer, intent(in) :: n                    ! Matrix dimension
      real(real64), intent(in) :: a(:)            ! Matrix values
      integer, intent(in) :: ia(:), ja(:)         ! Sparse indices
      real(real64), intent(in) :: b(:)            ! RHS vector
      real(real64), intent(out) :: x(:)           ! Solution vector
      type(solver_options_t), intent(in) :: opts
      type(solver_stats_t), intent(out) :: stat
    end subroutine solve_interface

    subroutine finalize_interface(this, stat)
      import :: solver_backend_t, solver_stats_t
      class(solver_backend_t), intent(inout) :: this
      type(solver_stats_t), intent(out) :: stat
    end subroutine finalize_interface

    logical function supports_interface(this, matrix_type)
      import :: solver_backend_t
      class(solver_backend_t), intent(in) :: this
      integer, intent(in) :: matrix_type
    end function supports_interface
  end interface

contains

  ! High-level solve routine (facade)
  subroutine solver_solve(n, a, ia, ja, b, x, opts, stat)
    integer, intent(in) :: n
    real(real64), intent(in) :: a(:), b(:)
    integer, intent(in) :: ia(:), ja(:)
    real(real64), intent(out) :: x(:)
    type(solver_options_t), intent(in) :: opts
    type(solver_stats_t), intent(out) :: stat

    class(solver_backend_t), allocatable :: backend

    ! Select backend based on opts%backend_id
    call create_backend(opts%backend_id, backend)

    ! Initialize, solve, finalize
    call backend%initialize(opts, stat)
    if (stat%status /= 0) return

    call backend%solve(n, a, ia, ja, b, x, opts, stat)

    call backend%finalize(stat)

  end subroutine solver_solve

end module solver_interface
```

---

## 4. Backend Registration System

### 4.1 Dynamic Backend Registry

```fortran
! src/core/solvers/solver_registry.f90
module solver_registry
  use solver_interface
  implicit none
  private
  public :: register_backend, get_backend, list_backends

  integer, parameter :: MAX_BACKENDS = 10

  type :: backend_entry_t
    integer :: id
    character(len=32) :: name
    class(solver_backend_t), allocatable :: backend
    logical :: available
  end type backend_entry_t

  type(backend_entry_t) :: registry(MAX_BACKENDS)
  integer :: num_registered = 0

contains

  subroutine register_backend(id, name, backend, available)
    integer, intent(in) :: id
    character(len=*), intent(in) :: name
    class(solver_backend_t), allocatable, intent(inout) :: backend
    logical, intent(in) :: available

    num_registered = num_registered + 1
    registry(num_registered)%id = id
    registry(num_registered)%name = name
    call move_alloc(backend, registry(num_registered)%backend)
    registry(num_registered)%available = available
  end subroutine register_backend

  subroutine get_backend(id, backend, found)
    integer, intent(in) :: id
    class(solver_backend_t), allocatable, intent(out) :: backend
    logical, intent(out) :: found

    integer :: i

    found = .false.
    do i = 1, num_registered
      if (registry(i)%id == id .and. registry(i)%available) then
        allocate(backend, source=registry(i)%backend)
        found = .true.
        return
      end if
    end do
  end subroutine get_backend

  subroutine list_backends(ids, names, available, count)
    integer, intent(out) :: ids(:)
    character(len=32), intent(out) :: names(:)
    logical, intent(out) :: available(:)
    integer, intent(out) :: count

    integer :: i
    count = min(num_registered, size(ids))
    do i = 1, count
      ids(i) = registry(i)%id
      names(i) = registry(i)%name
      available(i) = registry(i)%available
    end do
  end subroutine list_backends

end module solver_registry
```

---

## 5. Configuration Management

### 5.1 Configuration File Format

```ini
# nastran_solvers.conf
# NASTRAN-95 Solver Configuration

[default]
backend = pardiso          # native, pardiso, mumps, petsc, umfpack, superlu
matrix_type = symmetric    # general, symmetric, spd, complex
use_parallel = true
num_threads = 0            # 0 = automatic (OMP_NUM_THREADS)
verbose = false

[pardiso]
reuse_symbolic = true
iparm1 = 0                 # Use default PARDISO parameters
iparm3 = 0                 # Preconditioner (0=no, 1=ILU)

[mumps]
use_mpi = true
ordering = automatic       # automatic, amd, metis, scotch, parmetis
scaling = automatic
iterative_refinement = 2

[petsc]
ksp_type = gmres           # cg, gmres, bicgstab, preonly
pc_type = ilu              # ilu, jacobi, asm, hypre, none
ksp_rtol = 1.0e-8
ksp_max_it = 1000

[analysis.static]
# Override for static analysis
backend = mumps

[analysis.modal]
# Override for eigenvalue problems
backend = native           # Will be replaced by SLEPc later
```

### 5.2 Configuration Loader

```fortran
! src/core/solvers/solver_config.f90
module solver_config
  use solver_types
  implicit none
  private
  public :: load_config, get_options_for_analysis

contains

  subroutine load_config(filename, opts, stat)
    character(len=*), intent(in) :: filename
    type(solver_options_t), intent(out) :: opts
    integer, intent(out) :: stat

    ! Parse INI-style config file
    ! Set defaults, then override from file

    ! Defaults
    opts%backend_id = SOLVER_NATIVE
    opts%matrix_type = MATRIX_SYMMETRIC
    opts%storage_format = STORAGE_CSR
    opts%use_parallel = .false.
    opts%num_threads = 0
    opts%tolerance = 1.0e-8_real64
    opts%max_iterations = 1000
    opts%reuse_symbolic = .true.
    opts%verbose = .false.

    ! Parse file (implementation details omitted)
    ! ...

    stat = 0
  end subroutine load_config

  subroutine get_options_for_analysis(analysis_type, opts)
    character(len=*), intent(in) :: analysis_type
    type(solver_options_t), intent(out) :: opts

    ! Load default config
    call load_config('nastran_solvers.conf', opts, stat)

    ! Apply analysis-specific overrides
    select case (trim(analysis_type))
      case ('static')
        opts%backend_id = SOLVER_MUMPS
      case ('modal')
        opts%backend_id = SOLVER_NATIVE  ! TODO: Replace with SLEPc
      case ('frequency')
        opts%matrix_type = MATRIX_COMPLEX
        opts%backend_id = SOLVER_PARDISO
      case ('dynamic')
        opts%backend_id = SOLVER_PETSC
    end select
  end subroutine get_options_for_analysis

end module solver_config
```

---

## 6. Backend Implementations

### 6.1 Native Backend (FBS/DCOMP Wrapper)

```fortran
! src/core/solvers/backends/backend_native.f90
module backend_native
  use solver_interface
  use solver_types
  implicit none
  private
  public :: native_backend_t

  type, extends(solver_backend_t) :: native_backend_t
    ! Native solver state (if needed)
  contains
    procedure :: initialize => native_init
    procedure :: solve => native_solve
    procedure :: finalize => native_finalize
    procedure :: supports_matrix_type => native_supports
  end type native_backend_t

contains

  subroutine native_init(this, opts, stat)
    class(native_backend_t), intent(inout) :: this
    type(solver_options_t), intent(in) :: opts
    type(solver_stats_t), intent(out) :: stat

    this%name = 'NASTRAN Native (FBS/DCOMP)'
    this%backend_id = SOLVER_NATIVE
    this%supports_parallel = .false.
    this%supports_complex = .true.
    this%supports_iterative = .false.

    stat%status = 0
  end subroutine native_init

  subroutine native_solve(this, n, a, ia, ja, b, x, opts, stat)
    class(native_backend_t), intent(inout) :: this
    integer, intent(in) :: n
    real(real64), intent(in) :: a(:), b(:)
    integer, intent(in) :: ia(:), ja(:)
    real(real64), intent(out) :: x(:)
    type(solver_options_t), intent(in) :: opts
    type(solver_stats_t), intent(out) :: stat

    ! Call existing NASTRAN solvers
    ! This wraps the legacy FBS/DCOMP routines

    ! Example: Call legacy DCOMP + FBS
    ! CALL DCOMP(n, a, ia, ja, ...)
    ! CALL FBS(n, a, b, x, ...)

    stat%status = 0
    stat%time_solve = 0.0_real64  ! Measure actual time
  end subroutine native_solve

  subroutine native_finalize(this, stat)
    class(native_backend_t), intent(inout) :: this
    type(solver_stats_t), intent(out) :: stat
    stat%status = 0
  end subroutine native_finalize

  logical function native_supports(this, matrix_type)
    class(native_backend_t), intent(in) :: this
    integer, intent(in) :: matrix_type
    native_supports = .true.  ! Native supports all types
  end function native_supports

end module backend_native
```

### 6.2 PARDISO Backend

```fortran
! src/core/solvers/backends/backend_pardiso.f90
module backend_pardiso
  use solver_interface
  use solver_types
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: pardiso_backend_t

  type, extends(solver_backend_t) :: pardiso_backend_t
    ! PARDISO internal data structures
    integer(8) :: pt(64)      ! Internal solver memory pointer
    integer :: iparm(64)      ! PARDISO control parameters
    integer :: mtype          ! Matrix type (PARDISO encoding)
    integer :: phase          ! Current phase
  contains
    procedure :: initialize => pardiso_init
    procedure :: solve => pardiso_solve
    procedure :: finalize => pardiso_finalize
    procedure :: supports_matrix_type => pardiso_supports
  end type pardiso_backend_t

  ! PARDISO external interface (FORTRAN)
  external :: pardiso

contains

  subroutine pardiso_init(this, opts, stat)
    class(pardiso_backend_t), intent(inout) :: this
    type(solver_options_t), intent(in) :: opts
    type(solver_stats_t), intent(out) :: stat

    this%name = 'Intel MKL PARDISO'
    this%backend_id = SOLVER_PARDISO
    this%supports_parallel = .true.
    this%supports_complex = .true.
    this%supports_iterative = .false.

    ! Initialize PARDISO
    this%pt = 0
    this%iparm = 0

    ! Set matrix type
    select case (opts%matrix_type)
      case (MATRIX_SPD)
        this%mtype = 2   ! Real SPD
      case (MATRIX_SYMMETRIC)
        this%mtype = -2  ! Real symmetric indefinite
      case (MATRIX_GENERAL)
        this%mtype = 11  ! Real unsymmetric
      case (MATRIX_COMPLEX)
        this%mtype = 13  ! Complex unsymmetric
    end select

    ! Set OpenMP threads
    if (opts%num_threads > 0) then
      call mkl_set_num_threads(opts%num_threads)
    end if

    ! Configure PARDISO parameters
    this%iparm(1) = 1   ! Use custom parameters
    this%iparm(3) = opts%num_threads
    this%iparm(8) = 2   ! Iterative refinement steps

    stat%status = 0
  end subroutine pardiso_init

  subroutine pardiso_solve(this, n, a, ia, ja, b, x, opts, stat)
    class(pardiso_backend_t), intent(inout) :: this
    integer, intent(in) :: n
    real(real64), intent(in) :: a(:), b(:)
    integer, intent(in) :: ia(:), ja(:)
    real(real64), intent(out) :: x(:)
    type(solver_options_t), intent(in) :: opts
    type(solver_stats_t), intent(out) :: stat

    integer :: maxfct, mnum, nrhs, msglvl, error
    real(real64) :: ddum(1)
    integer :: idum(1)

    maxfct = 1
    mnum = 1
    nrhs = 1
    msglvl = merge(1, 0, opts%verbose)

    ! Phase 1: Analysis (symbolic factorization)
    this%phase = 11
    call pardiso(this%pt, maxfct, mnum, this%mtype, this%phase, &
                 n, a, ia, ja, idum, nrhs, this%iparm, msglvl, &
                 ddum, ddum, error)

    if (error /= 0) then
      stat%status = error
      return
    end if

    ! Phase 2: Numerical factorization
    this%phase = 22
    call pardiso(this%pt, maxfct, mnum, this%mtype, this%phase, &
                 n, a, ia, ja, idum, nrhs, this%iparm, msglvl, &
                 ddum, ddum, error)

    if (error /= 0) then
      stat%status = error
      return
    end if

    ! Phase 3: Solve
    this%phase = 33
    x = b  ! Copy RHS to solution vector
    call pardiso(this%pt, maxfct, mnum, this%mtype, this%phase, &
                 n, a, ia, ja, idum, nrhs, this%iparm, msglvl, &
                 b, x, error)

    stat%status = error
    stat%iterations = this%iparm(7)  ! Iterative refinement steps
  end subroutine pardiso_solve

  subroutine pardiso_finalize(this, stat)
    class(pardiso_backend_t), intent(inout) :: this
    type(solver_stats_t), intent(out) :: stat

    integer :: maxfct, mnum, nrhs, msglvl, error
    real(real64) :: ddum(1)
    integer :: idum(1)

    ! Release internal memory
    this%phase = -1
    maxfct = 1
    mnum = 1
    nrhs = 1
    msglvl = 0

    call pardiso(this%pt, maxfct, mnum, this%mtype, this%phase, &
                 1, ddum, idum, idum, idum, nrhs, this%iparm, msglvl, &
                 ddum, ddum, error)

    stat%status = error
  end subroutine pardiso_finalize

  logical function pardiso_supports(this, matrix_type)
    class(pardiso_backend_t), intent(in) :: this
    integer, intent(in) :: matrix_type
    pardiso_supports = .true.  ! PARDISO supports all types
  end function pardiso_supports

end module backend_pardiso
```

---

## 7. Integration with Existing Code

### 7.1 Drop-in Replacement Pattern

**Before (legacy code):**
```fortran
! In static analysis module
CALL DCOMP(N, A, IA, JA, IPARM, ...)
CALL FBS(N, A, B, X, ...)
```

**After (with abstraction):**
```fortran
use solver_interface
use solver_types
use solver_config

type(solver_options_t) :: opts
type(solver_stats_t) :: stat

! Load solver config for static analysis
call get_options_for_analysis('static', opts)

! Solve (backend selected automatically)
call solver_solve(n, a, ia, ja, b, x, opts, stat)

if (stat%status /= 0) then
  write(*,*) 'Solver failed with status', stat%status
  stop
end if
```

### 7.2 Backward Compatibility Wrapper

For minimal code changes, provide wrapper subroutines:

```fortran
! src/core/solvers/legacy_wrapper.f90
module legacy_solver_wrapper
  use solver_interface
  implicit none

contains

  ! Drop-in replacement for FBS
  subroutine FBS_NEW(N, A, IA, JA, B, X, IERR)
    integer, intent(in) :: N
    real(real64), intent(in) :: A(:), B(:)
    integer, intent(in) :: IA(:), JA(:)
    real(real64), intent(out) :: X(:)
    integer, intent(out) :: IERR

    type(solver_options_t) :: opts
    type(solver_stats_t) :: stat

    ! Use configured default solver
    call get_options_for_analysis('static', opts)
    call solver_solve(n, a, ia, ja, b, x, opts, stat)

    IERR = stat%status
  end subroutine FBS_NEW

end module legacy_solver_wrapper
```

---

## 8. CMake Build Integration

### 8.1 CMakeLists.txt Configuration

```cmake
# src/core/solvers/CMakeLists.txt

# Solver abstraction core (always built)
add_library(nastran_solver_interface
  solver_types.f90
  solver_interface.f90
  solver_registry.f90
  solver_config.f90
)

target_include_directories(nastran_solver_interface
  PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY}
)

# Native backend (always available)
add_library(nastran_solver_native
  backends/backend_native.f90
)
target_link_libraries(nastran_solver_native
  PRIVATE nastran_solver_interface
)

# PARDISO backend (if MKL available)
if(USE_MKL_PARDISO AND BLAS_FOUND)
  add_library(nastran_solver_pardiso
    backends/backend_pardiso.f90
  )
  target_link_libraries(nastran_solver_pardiso
    PRIVATE nastran_solver_interface
    PRIVATE ${BLAS_LIBRARIES}
  )
  target_compile_definitions(nastran_solver_pardiso
    PRIVATE -DHAVE_PARDISO=1
  )
endif()

# MUMPS backend (if MUMPS available)
if(USE_MUMPS AND MUMPS_FOUND)
  add_library(nastran_solver_mumps
    backends/backend_mumps.f90
  )
  target_link_libraries(nastran_solver_mumps
    PRIVATE nastran_solver_interface
    PRIVATE MUMPS::MUMPS
  )
  target_compile_definitions(nastran_solver_mumps
    PRIVATE -DHAVE_MUMPS=1
  )
endif()

# Combined solver library
add_library(nastran_solvers)
target_link_libraries(nastran_solvers
  PUBLIC nastran_solver_interface
  PUBLIC nastran_solver_native
)

if(TARGET nastran_solver_pardiso)
  target_link_libraries(nastran_solvers PUBLIC nastran_solver_pardiso)
endif()

if(TARGET nastran_solver_mumps)
  target_link_libraries(nastran_solvers PUBLIC nastran_solver_mumps)
endif()
```

### 8.2 Compile-Time Configuration

```fortran
! Auto-generated: src/core/solvers/solver_config_auto.f90
module solver_config_auto
  implicit none

#ifdef HAVE_PARDISO
  logical, parameter :: HAVE_PARDISO_BACKEND = .true.
#else
  logical, parameter :: HAVE_PARDISO_BACKEND = .false.
#endif

#ifdef HAVE_MUMPS
  logical, parameter :: HAVE_MUMPS_BACKEND = .true.
#else
  logical, parameter :: HAVE_MUMPS_BACKEND = .false.
#endif

#ifdef HAVE_PETSC
  logical, parameter :: HAVE_PETSC_BACKEND = .true.
#else
  logical, parameter :: HAVE_PETSC_BACKEND = .false.
#endif

end module solver_config_auto
```

---

## 9. Usage Examples

### 9.1 Runtime Solver Selection

```fortran
program test_solvers
  use solver_interface
  use solver_types
  implicit none

  integer, parameter :: n = 1000
  real(real64) :: a(5000), b(n), x(n)
  integer :: ia(n+1), ja(5000)
  type(solver_options_t) :: opts
  type(solver_stats_t) :: stat

  ! Build test matrix (SPD)
  call build_test_matrix(n, a, ia, ja)
  b = 1.0_real64

  ! Test 1: Native solver
  print *, 'Testing native solver...'
  opts%backend_id = SOLVER_NATIVE
  call solver_solve(n, a, ia, ja, b, x, opts, stat)
  print *, 'Status:', stat%status, ' Time:', stat%time_solve

  ! Test 2: PARDISO
  print *, 'Testing PARDISO...'
  opts%backend_id = SOLVER_PARDISO
  opts%matrix_type = MATRIX_SPD
  opts%use_parallel = .true.
  call solver_solve(n, a, ia, ja, b, x, opts, stat)
  print *, 'Status:', stat%status, ' Time:', stat%time_solve

  ! Test 3: MUMPS (if available)
  if (HAVE_MUMPS_BACKEND) then
    print *, 'Testing MUMPS...'
    opts%backend_id = SOLVER_MUMPS
    call solver_solve(n, a, ia, ja, b, x, opts, stat)
    print *, 'Status:', stat%status, ' Time:', stat%time_solve
  end if

end program test_solvers
```

### 9.2 Config File Selection

```bash
# nastran_solvers.conf
[default]
backend = pardiso

[analysis.static]
backend = mumps
use_parallel = true

[analysis.modal]
backend = native  # Until SLEPc integrated
```

```fortran
! In analysis module
call get_options_for_analysis('static', opts)
call solver_solve(n, a, ia, ja, b, x, opts, stat)
! Automatically uses MUMPS for static analysis
```

---

## 10. Testing Strategy

### 10.1 Test Matrix Suite

```
tests/golden_matrices/
├── spd_10x10.mtx          # Small SPD matrix
├── spd_1000x1000.mtx      # Medium SPD
├── unsym_500x500.mtx      # Unsymmetric
├── complex_100x100.mtx    # Complex matrix
└── solutions/
    ├── spd_10x10_sol.vec
    └── ...
```

### 10.2 Test Harness

```fortran
! tests/test_solver_interface.f90
subroutine test_all_solvers()
  integer :: i, n_backends
  integer :: ids(10)
  character(len=32) :: names(10)
  logical :: available(10)

  call list_backends(ids, names, available, n_backends)

  do i = 1, n_backends
    if (available(i)) then
      print *, 'Testing backend:', trim(names(i))
      call test_backend(ids(i))
    end if
  end do
end subroutine test_all_solvers
```

---

## 11. Performance Monitoring

### 11.1 Built-in Profiling

```fortran
if (opts%verbose) then
  print *, 'Solver:', backend%name
  print *, 'Matrix:', n, 'x', n, ', nnz=', size(a)
  print *, 'Symbolic time:', stat%time_symbolic, 's'
  print *, 'Numeric time:', stat%time_numeric, 's'
  print *, 'Solve time:', stat%time_solve, 's'
  print *, 'Memory used:', stat%memory_used / 1024**2, 'MB'
end if
```

### 11.2 Benchmark Mode

```bash
# Run with all available solvers and compare
./nastran --benchmark --input test.nas
```

Output:
```
Benchmark Results for test.nas (N=50000, NNZ=250000)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Backend    │ Time (s) │ Memory (MB) │ Speedup │ Status
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Native     │   45.23  │    1024     │  1.00x  │ ✓
PARDISO    │    4.12  │     128     │ 10.98x  │ ✓
MUMPS      │    2.87  │     156     │ 15.76x  │ ✓
PETSc      │    3.45  │     98      │ 13.11x  │ ✓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## 12. Migration Path

### Phase 1: Core Infrastructure (Week 1-2)
- [ ] Implement `solver_types.f90`
- [ ] Implement `solver_interface.f90` (abstract base)
- [ ] Implement `solver_registry.f90`
- [ ] Implement `solver_config.f90`
- [ ] Unit tests for infrastructure

### Phase 2: Native Backend (Week 2-3)
- [ ] Wrap existing FBS/DCOMP in `backend_native.f90`
- [ ] Integration tests with existing solvers
- [ ] Validation against baseline results

### Phase 3: PARDISO Backend (Week 3-4)
- [ ] Implement `backend_pardiso.f90`
- [ ] CMake detection of MKL PARDISO
- [ ] Performance benchmarking

### Phase 4: Integration (Week 4-5)
- [ ] Update analysis modules to use new interface
- [ ] Config file system
- [ ] Documentation and examples

### Phase 5: Additional Backends (Week 6+)
- [ ] MUMPS backend
- [ ] UMFPACK backend (C wrapper)
- [ ] PETSc backend
- [ ] SLEPc eigenvalue backend

---

## 13. Benefits Summary

✅ **Pluggable**: Add new solvers without touching existing code
✅ **Flexible**: Runtime solver selection via config file
✅ **Performance**: Choose best solver for each problem type
✅ **Testable**: Easy to benchmark and validate
✅ **Maintainable**: Clean separation of concerns
✅ **Future-proof**: Easy to add GPU, distributed solvers later
✅ **Backward compatible**: Legacy code works with wrappers

---

**Next Steps:** Begin implementation of Phase 1 (core infrastructure)?
