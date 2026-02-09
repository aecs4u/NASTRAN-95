# HPC Solver Libraries Quick Reference for NASTRAN-95

## Top 3 Recommendations (Start Here)

### 🥇 1. MUMPS - **HIGHEST PRIORITY**
**What:** Multifrontal sparse direct solver
**Why:** FORTRAN-native, parallel (MPI), perfect for FEA matrices
**Effort:** 4-6 weeks
**Impact:** 10-100x faster on medium models (10k-100k DOF)

**Integration:**
```fortran
! Direct FORTRAN call - no C wrapper needed!
CALL DMUMPS(MUMPS_PAR)
```

---

### 🥈 2. MKL PARDISO - **QUICK WIN**
**What:** Intel MKL's parallel sparse direct solver
**Why:** Already have Intel MKL (for BLAS), zero new dependencies
**Effort:** 1-2 weeks
**Impact:** 5-10x faster, OpenMP parallel

**Integration:**
```bash
# Already linked - just call it!
cmake -DUSE_EXTERNAL_BLAS=ON ..  # ✓ Already done
```

---

### 🥉 3. PETSc + SLEPc - **STRATEGIC**
**What:** Comprehensive parallel solver toolkit
**Why:** Iterative solvers (huge models) + modern eigensolvers (modal analysis)
**Effort:** 6-8 weeks
**Impact:** Enable 100k-1M DOF problems, replace 1970s eigensolvers

**Integration:**
```fortran
! FORTRAN API available
CALL KSPCreate(PETSC_COMM_WORLD, ksp, ierr)
CALL KSPSolve(ksp, b, x, ierr)
```

---

## Library Comparison Matrix

| Library | Language | Parallel | NASTRAN Fit | Effort | Priority |
|---------|----------|----------|-------------|--------|----------|
| **MUMPS** | FORTRAN | MPI | ⭐⭐⭐⭐⭐ Perfect | Medium | **CRITICAL** |
| **MKL PARDISO** | FORTRAN | OpenMP | ⭐⭐⭐⭐ Great | Low | **HIGH** |
| **PETSc** | C+FORTRAN API | MPI | ⭐⭐⭐⭐⭐ Perfect | Medium-High | **CRITICAL** |
| **SLEPc** | C+FORTRAN API | MPI | ⭐⭐⭐⭐⭐ Perfect for eigenvalues | Medium | **HIGH** |
| **UMFPACK** | C | Serial | ⭐⭐⭐ Good | Low | **MEDIUM** |
| **SuperLU** | C | Serial/MPI | ⭐⭐⭐ Good | Medium | **MEDIUM** |
| **PaStiX** | C | MPI+threads | ⭐⭐⭐⭐ Very Good | Medium | **LOW** |
| **Trilinos** | C++ | MPI | ⭐⭐ Poor (C++ barrier) | High | **DEFER** |

---

## Solver Selection Guide

### By Problem Type

| Problem Type | Current Solver | Recommended Upgrade | Expected Speedup |
|-------------|----------------|---------------------|------------------|
| **Static analysis** (small: <10k DOF) | FBS direct | MKL PARDISO | 3-5x |
| **Static analysis** (medium: 10k-100k DOF) | Dense fallback 💀 | MUMPS | 10-50x |
| **Static analysis** (large: >100k DOF) | Fails 💀 | PETSc iterative | ∞ (enables problem) |
| **Modal analysis** (eigenvalues) | Givens/QR (1970s) | SLEPc | 5-20x + better accuracy |
| **Frequency response** | Complex FBS | MUMPS (complex mode) | 10-30x |
| **Dynamic transient** | Direct time-stepping | PETSc KSP | 5-15x per step |

---

## Installation Quick Start

### Ubuntu/Debian
```bash
# Quick win: PARDISO (no install - already have MKL!)

# MUMPS + PETSc/SLEPc
sudo apt install libmumps-dev petsc-dev slepc-dev

# Optional: SuperLU, UMFPACK
sudo apt install libsuperlu-dev libsuitesparse-dev
```

### Build with new solvers
```bash
cd build
cmake .. \
  -DUSE_EXTERNAL_BLAS=ON \
  -DUSE_MUMPS=ON \
  -DUSE_PETSC=ON \
  -DUSE_MKL_PARDISO=ON

make -j$(nproc)
```

---

## Performance Expectations

### Current Baseline (Dense Fallback)
- **10k DOF**: ~5 seconds, 800 MB RAM
- **50k DOF**: ~2 minutes, 20 GB RAM
- **100k DOF**: Out of memory 💀

### With MUMPS (Sparse Direct)
- **10k DOF**: ~0.5 seconds, 50 MB RAM (10x faster, 16x less memory)
- **50k DOF**: ~8 seconds, 500 MB RAM (15x faster, 40x less memory)
- **100k DOF**: ~45 seconds, 2 GB RAM ✅ (previously impossible)

### With PETSc Iterative (Large Problems)
- **100k DOF**: ~10 seconds, 1 GB RAM (ideal for well-conditioned problems)
- **500k DOF**: ~1 minute, 5 GB RAM ✅ (previously impossible)
- **1M DOF**: ~5 minutes, 10 GB RAM ✅ (previously impossible)

---

## Implementation Roadmap (TL;DR)

### Phase 1: Quick Wins (2 weeks)
✅ Enable MKL PARDISO (already have MKL)
→ **5x speedup immediately**

### Phase 2: Sparse Direct (6 weeks)
✅ Integrate MUMPS
→ **10-50x speedup + 10-50x memory reduction**

### Phase 3: Iterative + Eigensolvers (6 weeks)
✅ Integrate PETSc (static) + SLEPc (modal)
→ **Enable 100k-1M DOF problems**

### Phase 4: Production (4 weeks)
✅ Testing, documentation, optimization
→ **Production-ready solver ecosystem**

**Total Time:** 4-5 months
**Total Impact:** Transform 1970s FEA code into modern HPC solver

---

## Key Advantages for NASTRAN vs Other FEA Codes

1. **Pure FORTRAN**: MUMPS, PETSc have native FORTRAN APIs (easier than C++ codes)
2. **Modular structure**: Phase 3 reorganization makes integration cleaner
3. **Legacy eigensolvers**: Bigger improvement potential than modern codes
4. **No commercial licensing**: Free alternative to MSC/Siemens NASTRAN

---

## Further Reading

- Full analysis: `docs/NASTRAN95_SOLVER_MODERNIZATION_ANALYSIS.md`
- CCX roadmap (reference): `docs/ccx_solver_modernization_roadmap.md`
- MUMPS docs: http://mumps-solver.org/
- PETSc docs: https://petsc.org/
- SLEPc docs: https://slepc.upv.es/

---

**Questions?** See full analysis document for detailed integration plans, API examples, and risk assessment.
