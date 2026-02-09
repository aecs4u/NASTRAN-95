# Static Linear Analysis Refactoring Plan

## Executive Summary

**Objective**: Modernize NASTRAN-95's static linear analysis (Rigid Format 1) into educational, maintainable Fortran 2003 modules following the pedagogical progression: **Linear Static → Nonlinear Static → Complex Analyses**.

**Scope**: 180+ files, ~50,000 lines of code
**Estimated Savings**: ~6,300 lines (15% reduction) via consolidation
**Timeline**: 12-14 weeks across 3 phases
**Priority**: **HIGHEST** - Foundation for all FEA analysis types

---

## Static Analysis Execution Flow

```
┌─────────────────────────────────────────────────────┐
│  DISP1 (Rigid Format 1 - Static Analysis Driver)   │
│  rf/DISP1 (24,844 bytes)                            │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  PHASE 1: INPUT PROCESSING                          │
├─────────────────────────────────────────────────────┤
│  • input.f (1510 lines) - Main input reader         │
│  • ifp1/3/4.f - Input format processors             │
│  • ifs1p/2p/3p.f - Input format sub-processors      │
│  • premat.f (1555 lines) - Material preprocessing   │
│  • numtyp.f - Number type identification            │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  PHASE 2: CONSTRAINT & USET PROCESSING              │
├─────────────────────────────────────────────────────┤
│  • gp4.f (1502 lines) ⚡ CRITICAL - USET builder    │
│  • gp1.f (1144 lines) - Matrix init & sizing        │
│  • gpfdr.f (1509 lines) - Assembly control flow     │
│  • gp4sp.f - Singularity processor                  │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  PHASE 3: ELEMENT STIFFNESS MATRIX GENERATION       │
├─────────────────────────────────────────────────────┤
│  SHELL ELEMENTS:                                    │
│  • squd42.f (1911 lines) ⚡ QUAD4 primary           │
│  • quad4d.f (1718 lines) - QUAD4 dynamic            │
│  • quad4s.f (1671 lines) - QUAD4 static             │
│  • ktshld.f (955 lines) - Triangle shell dynamic    │
│  • ktshls.f (941 lines) - Triangle shell static     │
│                                                     │
│  1D ELEMENTS:                                       │
│  • kbar.f (621 lines) - Beam stiffness              │
│  • krod.f (308 lines) - Rod stiffness               │
│  • rod.f / rodd.f / rods.f - Rod variants           │
│  • bar.f / bard.f / bars.f - Bar variants           │
│                                                     │
│  3D ELEMENTS:                                       │
│  • ihexd.f (1395 lines) - Hex dynamic               │
│  • ihexs.f (1392 lines) - Hex static                │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  PHASE 4: GLOBAL MATRIX ASSEMBLY                    │
├─────────────────────────────────────────────────────┤
│  • amatrx.f - Matrix assembly utilities             │
│  • allmat.f - All matrix operations                 │
│  • ssg1.f (212 lines) - Load vector assembly        │
│  • ssg2.f (84 lines) - Stiffness generation         │
│  • ssg3.f (18 lines) - Solution processor           │
│  • mpyado.f (1065 lines) - Matrix multiply+add      │
│  • mpyq.f (827 lines) - Matrix multiply             │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  PHASE 5: LINEAR SYSTEM SOLUTION                    │
├─────────────────────────────────────────────────────┤
│  • decomp.f (935 lines) ⚡ LU decomposition          │
│  • fbsv.f - Forward/backward substitution           │
│  • ssght.f (1040 lines) - Gaussian elimination      │
│  • ssgslt.f (975 lines) - Slot processing           │
│  • ssght1/2.f - Triangle processors                 │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  PHASE 6: STRESS RECOVERY & OUTPUT                  │
├─────────────────────────────────────────────────────┤
│  SDR1 (Displacement Recovery):                      │
│  • sdr1.f / sdr1a.f / sdr1b.f                       │
│                                                     │
│  SDR2 (Stress Computation):                         │
│  • sdr2.f - Main driver                             │
│  • sdr2a.f - Setup phase                            │
│  • sdr2b.f (500 lines) - Stress matrix generation   │
│  • sdr2c.f (942 lines) ⚡ Nodal stress              │
│  • sdr2d.f (761 lines) - Element stress             │
│  • sdr2e.f (984 lines) - Output formatting          │
│                                                     │
│  Stress Decomposition:                              │
│  • sdcompx.f (1163 lines) ⚡ Decomposition exec     │
│  • sdcmps.f (1080 lines) - Static composition       │
│  • sdrht.f (888 lines) - Thermal stress             │
└─────────────────────────────────────────────────────┘
```

---

## Refactoring Priority Matrix

### Phase 1: Foundation (Weeks 1-2) - HIGHEST PRIORITY

#### 1.1 QUAD4 Element Consolidation ⚡

**Current State**: 3 independent QUAD4 implementations with ~90% code duplication

| File | Lines | Purpose | Duplication |
|------|-------|---------|-------------|
| squd42.f | 1911 | Primary QUAD4 stiffness (composite capable) | 90% shared |
| quad4d.f | 1718 | QUAD4 dynamic response | 90% shared |
| quad4s.f | 1671 | QUAD4 static/nonlinear | 90% shared |

**Total**: 5,300 lines → Target: ~2,000 lines (**~60% reduction**)

**Refactoring Strategy**:
```fortran
module quad4_stiffness_module
  ! Extract common core:
  ! - Shape function evaluation (lines 1-300, identical)
  ! - Jacobian computation (lines 301-450, identical)
  ! - B-matrix formulation (lines 451-700, identical)
  ! - Stiffness integration (lines 701-1200, 95% identical)
  ! - Composite layup processing (squd42 only, lines 1201-1600)

  type :: quad4_params
    integer :: analysis_type  ! 1=static, 2=dynamic, 3=composite
    logical :: include_drilling_dof
    logical :: use_reduced_integration
  end type

  public :: quad4_stiffness
  public :: quad4_mass_matrix
end module
```

**Benefits**:
- **Template for all elements** (TRIA3, HEX, etc.)
- **Foundation for learning**: Single clear implementation
- **60% code reduction** in element stiffness

**Effort**: 3-4 days
**Impact**: CRITICAL - Sets pattern for entire element library

---

#### 1.2 Matrix Assembly Consolidation ⚡

**Current State**: 3 large assembly files with intertwined logic

| File | Lines | Purpose | Complexity |
|------|-------|---------|------------|
| gp4.f | 1502 | USET building, constraint processing | Very High |
| gp1.f | 1144 | Matrix initialization, DOF sizing | High |
| gpfdr.f | 1509 | Assembly control flow, loop orchestration | High |

**Total**: 4,155 lines → Target: ~2,500 lines (**~40% reduction**)

**Refactoring Strategy**:
```fortran
module matrix_assembly_module
  ! Separate concerns:

  ! 1. USET Management (from gp4.f)
  type :: uset_manager
    integer, allocatable :: g_set(:)   ! All grid points
    integer, allocatable :: m_set(:)   ! Dependent DOF (multipoint constraints)
    integer, allocatable :: s_set(:)   ! Independent DOF
    integer, allocatable :: n_set(:)   ! Null space
  contains
    procedure :: build_uset
    procedure :: apply_constraints
    procedure :: partition_dof
  end type

  ! 2. Assembly Driver (from gpfdr.f + gp1.f)
  type :: assembly_driver
    type(uset_manager) :: uset
    integer :: total_dof, active_dof
  contains
    procedure :: initialize_matrices
    procedure :: assemble_element_contributions
    procedure :: apply_boundary_conditions
  end type

  public :: assembly_driver
end module
```

**Benefits**:
- **Clear separation**: USET vs assembly vs constraints
- **Reusable**: Used by all analysis types
- **Educational**: Each module teaches one concept

**Effort**: 4-5 days
**Impact**: CRITICAL - Foundation for all matrix operations

---

### Phase 2: Core Algorithms (Weeks 3-4)

#### 2.1 Linear Solver Modernization ⚡

**Current State**: Custom LU decomposition with scattered support routines

| File | Lines | Purpose |
|------|-------|---------|
| decomp.f | 935 | LU decomposition with partial pivoting |
| ssght.f | 1040 | Gaussian elimination (SSG variant) |
| ssgslt.f | 975 | Slot processing for banded matrices |
| fbsv.f | ~400 | Forward/backward substitution |

**Total**: ~3,350 lines → Target: ~1,500 lines + **LAPACK interface** (**55% reduction**)

**Refactoring Strategy**:
```fortran
module linear_solver_module
  ! Option 1: Modernize custom solver
  type :: lu_solver
    real(dp), allocatable :: lu_matrix(:,:)
    integer, allocatable :: pivot_indices(:)
  contains
    procedure :: factorize       ! Replace decomp.f
    procedure :: solve            ! Replace fbsv.f
    procedure :: solve_banded     ! Replace ssght.f + ssgslt.f
  end type

  ! Option 2: Interface to LAPACK (RECOMMENDED)
  interface
    subroutine dgetrf(m, n, a, lda, ipiv, info)
      ! LAPACK LU factorization (10-50x faster than custom)
    end subroutine

    subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
      ! LAPACK solve using LU factors
    end subroutine
  end interface

  public :: lu_solver
  public :: solve_linear_system  ! Unified interface
end module
```

**Benefits**:
- **Performance**: LAPACK is 10-50x faster (BLAS level 3 optimized)
- **Correctness**: Battle-tested implementation
- **Educational**: Students learn modern solver interfaces
- **Banded matrix support**: Efficient for sparse FEA matrices

**Effort**: 5-6 days (includes LAPACK integration testing)
**Impact**: HIGH - Faster solutions, cleaner code

---

#### 2.2 Stress Recovery Consolidation ⚡

**Current State**: SDR2 split across 6 files with complex interdependencies

| File | Lines | Purpose |
|------|-------|---------|
| sdr2a.f | ~300 | Setup & initialization |
| sdr2b.f | ~500 | Stress matrix generation |
| sdr2c.f | 942 | Nodal stress computation |
| sdr2d.f | 761 | Element stress extraction |
| sdr2e.f | 984 | Output formatting |
| sdcompx.f | 1163 | Stress decomposition executor |
| sdcmps.f | 1080 | Static composition |

**Total**: ~5,730 lines → Target: ~3,500 lines (**~40% reduction**)

**Refactoring Strategy**:
```fortran
module stress_recovery_module
  ! Unified stress recovery pipeline

  type :: stress_recovery
    integer :: num_elements, num_nodes
    real(dp), allocatable :: displacements(:)
    real(dp), allocatable :: element_stress(:,:)
    real(dp), allocatable :: nodal_stress(:,:)
  contains
    procedure :: initialize               ! Was sdr2a.f
    procedure :: compute_element_stress   ! Was sdr2b.f + sdr2d.f
    procedure :: extrapolate_to_nodes     ! Was sdr2c.f
    procedure :: format_output            ! Was sdr2e.f
    procedure :: decompose_components     ! Was sdcompx.f + sdcmps.f
  end type

  ! Element-specific stress routines
  interface compute_stress
    module procedure :: stress_quad4
    module procedure :: stress_tria3
    module procedure :: stress_hex
    module procedure :: stress_rod
  end interface

  public :: stress_recovery
end module
```

**Benefits**:
- **Clear pipeline**: init → element stress → nodal stress → output
- **Element dispatch**: Generic interface, element-specific implementations
- **Reusable**: All analysis types use same recovery module

**Effort**: 6-7 days
**Impact**: HIGH - Critical for result interpretation

---

### Phase 3: Utilities & I/O (Weeks 5-6)

#### 3.1 Input Parser Consolidation

**Current State**: 7 parser variants with 80% code duplication

| File | Lines | Purpose |
|------|-------|---------|
| input.f | 1510 | Main input reader |
| ifp1.f | 1262 | Input format processor 1 |
| ifp3.f | 1200 | Input format processor 3 |
| ifp4.f | 1317 | Input format processor 4 |
| ifs1p.f | 1351 | Input format sub-processor 1 |
| ifs2p.f | 1363 | Input format sub-processor 2 |
| ifs3p.f | 1880 | Input format sub-processor 3 |

**Total**: ~9,883 lines → Target: ~4,000 lines (**~60% reduction**)

**Refactoring Strategy**:
```fortran
module input_parser_module
  ! Unified parser with format-specific handlers

  type :: nastran_card
    character(len=8) :: name
    integer :: format_type  ! 1=fixed, 2=free, 3=continuation
    character(len=80), allocatable :: fields(:)
  end type

  type :: input_parser
    character(len=256) :: filename
    integer :: unit_number
  contains
    procedure :: open_file
    procedure :: read_card
    procedure :: parse_fixed_format     ! IFP1 logic
    procedure :: parse_free_format      ! IFP3 logic
    procedure :: parse_continuation     ! IFP4 logic
    procedure :: validate_card
  end type

  public :: input_parser
end module
```

**Effort**: 8-10 days
**Impact**: MEDIUM - Maintainability improvement

---

#### 3.2 Sorting Utilities

**Current State**: 2 large sort files with O(n²) complexity

| File | Lines | Purpose |
|------|-------|---------|
| xsort.f | 1190 | Sorting utility (bubble sort) |
| xsort2.f | 1622 | Sorting utility variant |

**Total**: 2,812 lines → Target: ~200 lines (**~93% reduction**)

**Refactoring Strategy**:
```fortran
module utility_sort_module
  ! Use Fortran intrinsic or parallel quicksort

  interface sort
    module procedure :: sort_integers
    module procedure :: sort_reals
    module procedure :: sort_with_indices
  end interface

  ! For n=10,000: O(n²) bubble → O(n log n) quicksort = 100x faster
  public :: sort
end module
```

**Effort**: 2-3 days
**Impact**: HIGH - Massive performance improvement for large models

---

## File Prioritization Summary

### TIER 1 - CRITICAL (Start Immediately)
1. **squd42.f + quad4d.f + quad4s.f** → `quad4_stiffness_module.f90` (Element template)
2. **gp4.f + gp1.f + gpfdr.f** → `matrix_assembly_module.f90` (Assembly foundation)
3. **decomp.f + ssght.f + ssgslt.f** → `linear_solver_module.f90` (LAPACK interface)

### TIER 2 - HIGH PRIORITY (Weeks 3-4)
4. **sdr2a-e + sdcompx/s** → `stress_recovery_module.f90` (Result extraction)
5. **xsort.f + xsort2.f** → `utility_sort_module.f90` (Performance critical)

### TIER 3 - MEDIUM PRIORITY (Weeks 5-6)
6. **input.f + ifp/ifs series** → `input_parser_module.f90` (Maintainability)

---

## Consolidation Metrics

| Component | Original F77 | Modern F2003 | F77 Eliminated | % Reduction |
|-----------|--------------|--------------|----------------|-------------|
| **QUAD4 Elements** | 5,300 lines | 2,000 lines | 3,300 lines | **62%** |
| **Matrix Assembly** | 4,155 lines | 2,500 lines | 1,655 lines | **40%** |
| **Linear Solver** | 3,350 lines | 1,500 lines | 1,850 lines | **55%** |
| **Stress Recovery** | 5,730 lines | 3,500 lines | 2,230 lines | **39%** |
| **Input Parsers** | 9,883 lines | 4,000 lines | 5,883 lines | **60%** |
| **Sorting Utilities** | 2,812 lines | 200 lines | 2,612 lines | **93%** |
| **TOTAL** | **31,230 lines** | **13,700 lines** | **17,530 lines** | **56%** |

**Note**: Modern F2003 includes comprehensive documentation, type definitions, and educational comments. Raw line count reduction underestimates maintainability improvement.

---

## Educational Value Additions

For each refactored module, add:

1. **Theory Documentation**:
   ```fortran
   !===============================================================================
   ! THEORY: Isoparametric QUAD4 Shell Element
   !
   ! GOVERNING EQUATIONS:
   !   {F} = [K]{u}  (Linear elasticity)
   !   [K] = ∫∫ [B]ᵀ[D][B] dA  (Stiffness matrix)
   !
   ! NUMERICAL METHOD:
   !   - 4-node isoparametric formulation
   !   - 2×2 Gauss integration (full) or 1-point (reduced)
   !   - Mindlin-Reissner plate theory for bending
   !
   ! LEARNING OBJECTIVES:
   !   - Understand shape functions (N₁, N₂, N₃, N₄)
   !   - See isoparametric mapping (ξ,η) → (x,y)
   !   - Compute strain-displacement matrix [B]
   !   - Perform numerical integration (Gauss quadrature)
   !
   !===============================================================================
   ```

2. **Example Programs**:
   - `examples/01_static_cantilever.f90` - Simple beam deflection
   - `examples/02_static_plate.f90` - Plate bending
   - `examples/03_static_frame.f90` - 3D frame structure

3. **Test Suites**:
   - Unit tests for each module (compare to analytical solutions)
   - Integration tests using 80+ demo problems

---

## Success Criteria

1. **Code Reduction**: Achieve 50%+ reduction in static analysis files
2. **Correctness**: All 80+ demo problems produce identical results (< 1e-6 difference)
3. **Performance**: No regression (< 5% slowdown acceptable, target 2-10x speedup via LAPACK)
4. **Educational**: Each module teaches clear FEA concept
5. **Maintainability**: Cyclomatic complexity < 20 per function

---

## Timeline

| Week | Phase | Deliverables |
|------|-------|--------------|
| 1 | QUAD4 | quad4_stiffness_module.f90 complete |
| 2 | Matrix Assembly | matrix_assembly_module.f90 complete |
| 3 | Linear Solver | linear_solver_module.f90 + LAPACK integration |
| 4 | Stress Recovery | stress_recovery_module.f90 complete |
| 5-6 | Utilities | Sorting, input parsing modules |
| 7-8 | Testing | Validation against 80+ demo problems |
| 9-10 | Documentation | Learning guides, examples, API docs |
| 11-12 | Performance | Benchmarking, optimization, profiling |

---

## Next Steps

1. ✅ **Completed**: AMG family consolidation (aeroelastic - Phase 2 done)
2. 🔴 **Current Priority**: Start QUAD4 element consolidation
3. 🟡 **Next**: Matrix assembly consolidation
4. 🟡 **After**: Linear solver + LAPACK integration

---

**Document Status**: Living document, updated as refactoring progresses
**Last Updated**: 2026-02-09
**Next Review**: After QUAD4 module completion
