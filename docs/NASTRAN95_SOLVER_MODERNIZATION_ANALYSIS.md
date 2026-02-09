# NASTRAN-95 Solver Modernization Analysis
**Date:** 2026-02-09
**Based on:** CCX Solver Modernization Roadmap
**Scope:** Applicable HPC solver libraries for NASTRAN-95

---

## 1. Current NASTRAN-95 Solver Capabilities

### Existing Solvers (from source analysis)
Located in `src/core/solvers/` (41 files):

- **Direct solvers**: FBS (Forward-Backward Substitution), decomposition, factorization
- **Eigenvalue solvers**: Inverse iteration, Givens reduction, QR iteration
- **Matrix operations**: Decomposition (DCOMP), inversion (INVERT), determinant
- **Specialized**: Complex arithmetic solvers (CFBS), banded matrix solvers

### Current Limitations
1. **Dense matrix fallback**: Like CCX, likely converts sparse to dense for solve
2. **Single-threaded**: No parallel/distributed solve capability
3. **Limited iterative methods**: Primarily direct solvers
4. **Memory constraints**: Dense operations limit scalable problem sizes
5. **Legacy architecture**: 1970s-era algorithms, pre-modern sparse techniques

---

## 2. Recommended HPC Solver Libraries for NASTRAN-95

### Priority Classification

| Priority | Library | Rationale | Integration Complexity | Impact |
|----------|---------|-----------|----------------------|--------|
| **CRITICAL** | MUMPS | Industry standard, sparse direct, FORTRAN-native, parallel | Medium | Very High |
| **CRITICAL** | PETSc | Comprehensive toolkit, FORTRAN API, iterative+direct | Medium-High | Very High |
| **HIGH** | SuperLU/SuperLU_DIST | Proven sparse direct, scalable, C API | Medium | High |
| **HIGH** | UMFPACK (SuiteSparse) | Excellent sparse direct, well-documented | Low-Medium | High |
| **MEDIUM** | PaStiX | High-performance parallel, modern | Medium | High |
| **MEDIUM** | PARDISO (MKL) | Intel MKL integration (already using MKL BLAS) | Low | Medium |
| **LOW** | Trilinos | C++ framework, less FORTRAN-friendly | High | Medium |

---

## 3. Detailed Recommendations by Use Case

### 3.1 MUMPS (Multifrontal Massively Parallel Solver)
**Adoption Level:** CRITICAL - Highest Priority

**Why MUMPS for NASTRAN-95:**
- **FORTRAN-native**: Written in FORTRAN, seamless integration with NASTRAN's codebase
- **Sparse direct solver**: Handles typical FEA matrices (structural, modal) efficiently
- **MPI parallel**: Scales to large problems with distributed memory
- **Multifrontal method**: Ideal for finite element stiffness matrices
- **Symmetric/unsymmetric**: Supports both SPD and general matrices
- **Active maintenance**: Widely used in FEA community (Code_Aster, etc.)

**Integration Path:**
1. **Phase 1** (2-3 weeks): Interface layer in `src/core/solvers/mumps_interface.f`
2. **Phase 2** (1-2 weeks): Replace FBS calls in static analysis with MUMPS
3. **Phase 3** (1 week): Validation against existing test cases
4. **Phase 4** (1 week): Performance benchmarking and tuning

**API Example (FORTRAN):**
```fortran
! NASTRAN can call MUMPS directly
CALL DMUMPS(MUMPS_PAR)  ! Double precision
CALL ZMUMPS(MUMPS_PAR)  ! Complex (for frequency analysis)
```

**Benefits:**
- Handles 10x-100x larger problems than current dense fallback
- Parallel solve on multi-core/cluster
- Out-of-core capability for huge models
- Automatic pivot strategy

---

### 3.2 PETSc (Portable Extensible Toolkit for Scientific Computation)
**Adoption Level:** CRITICAL - Strategic Toolkit

**Why PETSc for NASTRAN-95:**
- **FORTRAN API**: Full FORTRAN interface (`petscvec.h`, `petscmat.h`, `petscksp.h`)
- **Comprehensive**: Iterative solvers (CG, GMRES, BiCGSTAB) + preconditioners (ILU, Jacobi, multigrid)
- **Scalable**: MPI-parallel, distributed sparse matrices
- **Solver zoo**: Access to MUMPS, SuperLU, PARDISO through unified interface
- **Eigenvalue support**: SLEPc (built on PETSc) for modal analysis modernization

**Integration Path:**
1. **Phase 1** (3-4 weeks): PETSc matrix assembly wrapper for NASTRAN sparse format
2. **Phase 2** (2-3 weeks): KSP solver integration for static analysis
3. **Phase 3** (2 weeks): EPS (SLEPc) integration for eigenvalue/modal analysis
4. **Phase 4** (2 weeks): Parallel assembly and solve optimization

**API Example (FORTRAN):**
```fortran
! NASTRAN can use PETSc's FORTRAN API
CALL MatCreate(PETSC_COMM_WORLD, A, ierr)
CALL KSPCreate(PETSC_COMM_WORLD, ksp, ierr)
CALL KSPSetOperators(ksp, A, A, ierr)
CALL KSPSolve(ksp, b, x, ierr)
```

**Benefits:**
- Future-proof: Active development, modern algorithms
- Flexibility: Switch between direct/iterative at runtime
- Preconditioners: Essential for ill-conditioned FEA matrices
- SLEPc: Modernize modal analysis (better than NASTRAN's 1970s eigensolvers)

---

### 3.3 SuperLU / SuperLU_DIST
**Adoption Level:** HIGH

**Why SuperLU for NASTRAN-95:**
- **Sparse LU factorization**: Direct replacement for NASTRAN's DCOMP/FBS
- **Sequential + distributed**: SuperLU (single-node), SuperLU_DIST (MPI parallel)
- **Mature**: Proven in FEA applications
- **C API**: Requires C-FORTRAN interface layer (manageable)

**Integration Path:**
1. **Phase 1** (2 weeks): C wrapper in `src/core/solvers/superlu_wrapper.c`
2. **Phase 2** (1-2 weeks): FORTRAN interface calls from analysis modules
3. **Phase 3** (1 week): Testing and validation

**Benefits:**
- Drop-in replacement for dense LU solver
- Excellent performance on unsymmetric matrices
- Widely available (part of many distros)

---

### 3.4 UMFPACK (SuiteSparse)
**Adoption Level:** HIGH

**Why UMFPACK for NASTRAN-95:**
- **Sparse LU for unsymmetric**: Gold standard for general sparse matrices
- **Easy integration**: Simple C API, clear documentation
- **Memory-efficient**: Highly optimized memory usage
- **Robust**: Handles near-singular matrices well

**Integration Path:**
1. **Phase 1** (1-2 weeks): C wrapper + FORTRAN interface
2. **Phase 2** (1 week): Integration into static analysis solver path
3. **Phase 3** (1 week): Validation

**Benefits:**
- Lightweight (no MPI dependency for initial adoption)
- Excellent for serial solve path
- Foundation for sparse backend abstraction

---

### 3.5 PaStiX
**Adoption Level:** MEDIUM (Optional, Performance-Oriented)

**Why PaStiX for NASTRAN-95:**
- **Hybrid parallel**: MPI + threads (matches modern HPC trends)
- **High performance**: Competitive with MUMPS on modern architectures
- **Low-rank compression**: Exploits matrix structure for memory savings

**Integration Path:**
1. **Phase 1** (2-3 weeks): Interface layer
2. **Phase 2** (1-2 weeks): Backend registration in solver abstraction
3. **Phase 3** (1 week): Performance comparison vs MUMPS

**Benefits:**
- Potentially faster than MUMPS on some problems
- Modern architecture awareness (NUMA, GPU offload future)

---

### 3.6 PARDISO (Intel MKL)
**Adoption Level:** MEDIUM (Low-Hanging Fruit)

**Why PARDISO for NASTRAN-95:**
- **Already available**: NASTRAN already links Intel MKL (for BLAS/LAPACK)
- **Zero new dependencies**: No additional libraries needed
- **Shared-memory parallel**: OpenMP threading (already enabled in build)
- **Easy integration**: Direct FORTRAN interface

**Integration Path:**
1. **Phase 1** (1 week): Interface to MKL's PARDISO from NASTRAN solvers
2. **Phase 2** (1 week): Testing and validation
3. **Phase 3** (3 days): Performance tuning (PARDISO parameters)

**Benefits:**
- Immediate availability (no build system changes)
- Good performance for symmetric/unsymmetric matrices
- OpenMP parallel without MPI complexity

**Caution:**
- Proprietary (Intel MKL license)
- Not available on non-Intel systems (ARM, RISC-V future)

---

### 3.7 Trilinos
**Adoption Level:** LOW (Future Consideration)

**Why NOT Trilinos (Initially):**
- **C++ framework**: Awkward integration with FORTRAN-77 codebase
- **Complexity**: Large dependency tree, heavyweight
- **Redundant**: PETSc provides similar capabilities with better FORTRAN support

**Potential Future Use:**
- If C++ modernization of NASTRAN core is ever undertaken
- For advanced capabilities (SNES nonlinear solvers, Tpetra distributed linear algebra)

---

## 4. Recommended Integration Strategy

### Phase A: Foundation (4-6 weeks)
**Goal:** Establish solver abstraction layer

1. **Create solver interface module** (`src/core/solvers/solver_interface.f90`)
   - Abstract solver API (FORTRAN 2003 interface)
   - Backend registration system
   - Solver selection logic (runtime configurable)

2. **Implement backend adapters:**
   - **Native**: Existing FBS/DCOMP solvers (baseline)
   - **PARDISO**: Quick win using existing MKL linkage
   - **UMFPACK**: Sparse LU baseline

3. **Validation framework:**
   - Golden test suite for solver parity
   - Numerical tolerance checks (relative/absolute error)
   - Regression detection

**Deliverables:**
- Solver abstraction layer compiles and passes tests
- PARDISO backend functional as proof-of-concept
- No regression in existing analysis modules

---

### Phase B: Sparse Direct Solvers (6-8 weeks)
**Goal:** Replace dense fallback with true sparse solvers

1. **MUMPS integration** (primary solver)
   - CMake FindMUMPS module
   - FORTRAN interface wrapper
   - Static analysis integration
   - Parallel solve configuration (MPI-aware build option)

2. **SuperLU integration** (secondary/fallback)
   - C wrapper + FORTRAN interface
   - Sequential solve path
   - Unsymmetric matrix specialization

3. **Testing and benchmarking:**
   - Test suite expansion (medium-large models)
   - Performance comparison: Native vs MUMPS vs SuperLU
   - Memory profiling (sparse vs dense)

**Deliverables:**
- MUMPS available as primary solver (CMake option)
- Documented performance improvements (solve time, memory)
- Validation report (numerical accuracy vs baseline)

---

### Phase C: Iterative Solvers + Eigenvalue (4-6 weeks)
**Goal:** Modernize modal analysis and enable iterative solvers

1. **PETSc/KSP integration**
   - Iterative solver path (CG, GMRES for static analysis)
   - Preconditioner selection (ILU, Jacobi, AMG)
   - Convergence monitoring

2. **SLEPc integration** (eigenvalue problems)
   - Replace legacy eigensolvers (Givens, inverse iteration)
   - Krylov-Schur, Arnoldi methods
   - Parallel eigensolve (MPI-enabled)

3. **Analysis module adaptation:**
   - Modal analysis (`src/analysis/modal/`) uses SLEPc
   - Dynamic analysis (`src/analysis/dynamic/`) uses PETSc KSP

**Deliverables:**
- PETSc-based iterative solvers functional
- SLEPc modal analysis validated against baseline
- User guide for solver selection (direct vs iterative)

---

### Phase D: Optimization + Production (3-4 weeks)
**Goal:** Production-ready, optimized, documented

1. **Performance tuning:**
   - Matrix assembly optimization (reduce overhead)
   - Solver parameter tuning (fill-reducing orderings, etc.)
   - Thread/MPI configuration guidelines

2. **User documentation:**
   - Solver selection guide (problem size, matrix type)
   - Build options (enabling MUMPS/PETSc/etc.)
   - Troubleshooting and debugging tips

3. **CI/CD integration:**
   - Test matrix (native, PARDISO, MUMPS, PETSc)
   - Nightly performance benchmarks
   - Regression alerts

**Deliverables:**
- Production-ready solver backends
- Comprehensive user/developer documentation
- Automated testing and validation

---

## 5. Build System Integration

### CMake Options (Proposed)

```cmake
# Solver backend options
option(USE_MUMPS "Enable MUMPS sparse direct solver" ON)
option(USE_SUPERLU "Enable SuperLU sparse solver" OFF)
option(USE_UMFPACK "Enable UMFPACK (SuiteSparse) solver" OFF)
option(USE_PETSC "Enable PETSc iterative solvers and SLEPc eigensolvers" OFF)
option(USE_PASTIX "Enable PaStiX parallel sparse solver" OFF)
option(USE_MKL_PARDISO "Use Intel MKL PARDISO (requires MKL)" ON)

# Parallelization
option(MUMPS_USE_MPI "Build MUMPS with MPI support" ON)
option(PETSC_USE_MPI "Build PETSc with MPI support" ON)

# Paths (if not auto-detected)
set(MUMPS_ROOT "" CACHE PATH "MUMPS installation directory")
set(PETSC_DIR "" CACHE PATH "PETSc installation directory")
set(SLEPC_DIR "" CACHE PATH "SLEPc installation directory")
```

### Dependency Management

**Preferred approach:** System packages
```bash
# Ubuntu/Debian
sudo apt install libmumps-dev libsuperlu-dev libsuitesparse-dev petsc-dev slepc-dev

# Fedora/RHEL
sudo dnf install MUMPS-devel SuperLU-devel suitesparse-devel petsc-devel slepc-devel

# macOS (Homebrew)
brew install mumps superlu suite-sparse petsc slepc
```

**Alternative:** Spack (HPC environments)
```bash
spack install mumps+mpi+metis+parmetis
spack install petsc+mumps+superlu-dist+hypre
spack install slepc
```

---

## 6. Success Metrics

### Performance Targets
- **Solve time**: 5-10x improvement on medium models (10k-100k DOF)
- **Memory usage**: 10-50x reduction (sparse vs dense)
- **Scalability**: 50-100k DOF problems solvable on single workstation
- **Parallel efficiency**: 70%+ efficiency on 4-8 cores (MUMPS/PETSc)

### Validation Criteria
- **Numerical accuracy**: <1e-6 relative error vs baseline (static analysis)
- **Eigenvalue accuracy**: <1e-4 relative error in frequencies (modal analysis)
- **Test coverage**: 100% of existing analysis paths validated
- **Regression**: Zero critical failures in CI test suite

---

## 7. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| FORTRAN-C interface bugs | Medium | High | Extensive wrapper testing, gradual rollout |
| MPI initialization conflicts | Low | Medium | Conditional MPI init, documentation |
| Numerical divergence | Low | High | Golden test suite, strict tolerances |
| Build complexity increase | High | Medium | Good CMake FindPackage modules, Docker images |
| Dependency availability | Medium | Medium | Spack integration, optional backends |

---

## 8. Comparison: NASTRAN vs CalculiX Modernization

### Similarities
- Both use legacy FORTRAN code (NASTRAN: F77, CalculiX: F77/F90)
- Both need sparse solver modernization (replace dense fallback)
- Both benefit from PETSc/MUMPS/SuperLU ecosystem

### Differences
| Aspect | NASTRAN-95 | CalculiX (CCX) |
|--------|------------|----------------|
| **Language** | Pure FORTRAN 77 | FORTRAN + C |
| **Integration** | Easier (FORTRAN-native libs) | Requires C wrappers |
| **Codebase age** | 1970s-1995 | 1990s-2020s |
| **MPI readiness** | None (serial only) | Partial (CalculiX-MPI fork exists) |
| **Eigensolvers** | Legacy (needs SLEPc badly) | Better (ARPACK integration exists) |

**Advantage for NASTRAN:** FORTRAN-native solvers (MUMPS, PETSc) integrate more naturally than in mixed C/FORTRAN codebases.

---

## 9. Immediate Next Actions (Priority Order)

### Week 1-2: Quick Wins
1. ✅ **Enable MKL PARDISO** (already have MKL BLAS)
   - Add solver interface in `src/core/solvers/pardiso_interface.f`
   - Test on small static analysis problems
   - Measure speedup vs native FBS

2. **Document current solver architecture**
   - Map call chains: analysis modules → solvers
   - Identify coupling points (where to inject new backends)
   - Create solver API design document (ADR)

### Week 3-4: Foundation
3. **Implement solver abstraction layer**
   - `solver_interface.f90` with abstract interface
   - Backend registration and selection logic
   - Compile-time and runtime configuration

4. **UMFPACK integration** (simplest external solver)
   - C wrapper in `src/core/solvers/umfpack_wrapper.c`
   - FORTRAN interface calls
   - Static analysis module integration

### Week 5-8: Core Modernization
5. **MUMPS integration** (primary solver)
   - CMake FindMUMPS.cmake module
   - FORTRAN interface in `src/core/solvers/mumps_interface.f`
   - Static + modal analysis integration
   - MPI-parallel configuration (optional)

6. **Validation and testing**
   - Expand test suite with medium models
   - Performance benchmarking framework
   - Golden comparison suite

### Week 9-12: Advanced Capabilities
7. **PETSc/SLEPc integration**
   - KSP solvers for static analysis
   - SLEPc eigensolvers for modal analysis
   - Iterative solver path (CG, GMRES)

8. **Documentation and release**
   - User guide (solver selection, build options)
   - Developer guide (adding new backends)
   - Performance report (benchmarks vs baseline)

---

## 10. Long-Term Vision (12-24 months)

### Solver Ecosystem
- **Default**: MUMPS (sparse direct, parallel)
- **Fallback**: UMFPACK (sparse direct, serial)
- **Iterative**: PETSc KSP (large problems, iterative)
- **Eigenvalue**: SLEPc (modal analysis, scalable)
- **Performance**: PaStiX (optional, high-end HPC)
- **Proprietary**: MKL PARDISO (Intel platforms)

### Problem Size Scaling
- **Current**: 1k-10k DOF (dense limit)
- **Phase B**: 10k-100k DOF (sparse direct)
- **Phase C**: 100k-1M DOF (iterative + MPI)
- **Future**: 1M-10M DOF (distributed solvers, GPU offload)

### Community Impact
- **Modern NASTRAN**: Bring 1970s FEA code into 2020s performance class
- **Open-source**: Free alternative to commercial NASTRAN (MSC, Siemens)
- **Educational**: Teach modern FEA solver techniques in legacy codebase
- **Preservation**: Keep historic NASA software relevant and usable

---

## 11. Conclusion

**Recommended Adoption Priority:**

1. **MUMPS** (highest priority, best FORTRAN integration, parallel)
2. **MKL PARDISO** (quick win, already have MKL)
3. **UMFPACK** (simple integration, serial baseline)
4. **PETSc + SLEPc** (strategic, comprehensive toolkit)
5. **SuperLU** (alternative sparse direct)
6. **PaStiX** (optional, performance-focused)

**Estimated Total Effort:** 16-24 weeks (4-6 months) for Phases A-D

**Expected Impact:**
- 10-100x speedup on medium-large models
- 10-50x memory reduction
- Enable problems previously unsolvable on single workstation
- Modernize 50-year-old solver technology with state-of-the-art HPC methods

**Alignment with CCX Roadmap:** High overlap in solver selection, integration patterns, and validation approach. NASTRAN can leverage CCX's lessons learned while benefiting from superior FORTRAN integration.
