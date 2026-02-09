# NASTRAN-95 Stub Functions - Implementation Status

## Overview

During the compilation phase, several functions were found to be missing from the codebase. Stub implementations were created to allow linking to complete. This document catalogs all stub functions and their implementation status.

**Total Stub Functions Created**: 26
**Implementation Status**: All are empty stubs requiring proper implementation
**Impact**: Executable compiles but may fail at runtime when stubs are called

---

## Critical Stubs (High Priority - Prevent Execution)

These functions are called during normal program execution and will cause failures if not implemented.

### 1. **DSSIZE** - Sparse Matrix Size Determination
**File**: `mis/dssize.f`
**Purpose**: Determine the size of sparse matrix data structures
**Called By**: Sparse matrix solvers
**Signature**:
```fortran
SUBROUTINE DSSIZE (FILE, NCOLS, NTERMS, NSTRGS, NWD)
  INTEGER FILE, NCOLS, NTERMS, NSTRGS, NWD
```

**Expected Behavior**:
- Read sparse matrix header from FILE
- Return number of columns (NCOLS)
- Return number of non-zero terms (NTERMS)
- Return number of strings/rows (NSTRGS)
- Return number of words (NWD)

**Current Implementation**: Returns zeros for all outputs

**Implementation Priority**: **CRITICAL** - Required for sparse matrix operations

---

### 2. **GETSTB** - Get Scratch Table Block
**File**: `mis/getstb.f`
**Purpose**: Allocate and retrieve a scratch table block from memory
**Called By**: Matrix assembly routines
**Signature**:
```fortran
SUBROUTINE GETSTB (NAME, NROWS, NCOLS, PREC, FILE, ICORE)
  INTEGER NAME(2), NROWS, NCOLS, PREC, FILE, ICORE
```

**Expected Behavior**:
- Allocate scratch memory for temporary table
- Set up table with NAME, NROWS, NCOLS
- Handle precision (PREC: 1=single, 2=double)
- Return FILE handle and core address (ICORE)

**Current Implementation**: No-op, returns without action

**Implementation Priority**: **CRITICAL** - Required for element assembly

---

### 3. **ENDGTB** - End Get Table Block
**File**: `mis/endgtb.f`
**Purpose**: Release scratch table block
**Called By**: Paired with GETSTB
**Signature**:
```fortran
SUBROUTINE ENDGTB (FILE)
  INTEGER FILE
```

**Expected Behavior**:
- Release memory associated with FILE
- Update free memory tracking

**Current Implementation**: No-op

**Implementation Priority**: **HIGH** - Memory management

---

## I/O and Data Management Stubs

### 4. **FINWRT** - Finish Write Operation
**File**: `mis/finwrt.f`
**Purpose**: Complete pending write operations, flush buffers
**Implementation Priority**: **HIGH**

---

### 5. **TRANSP** - Transpose Matrix
**File**: `mis/transp.f`
**Purpose**: Transpose matrix in place or to new location
**Implementation Priority**: **MEDIUM** - May have alternative implementation

---

### 6. **ONETWO** - One-to-Two Transformation
**File**: `mis/onetwo.f`
**Purpose**: Convert single-precision to double-precision (or similar transformation)
**Implementation Priority**: **MEDIUM**

---

### 7. **SDCMPS** - Single Precision Decompose
**File**: `mis/sdcmps.f`
**Purpose**: Single-precision matrix decomposition
**Implementation Priority**: **HIGH** - Core solver functionality

---

### 8. **APDB** - Aerodynamic Panel Database
**File**: `mis/apdb.f`
**Purpose**: Manage aerodynamic panel data
**Implementation Priority**: **LOW** - Only for aeroelastic analysis

---

## Input/Output Stubs

### 9. **INPTT5** - Input Type 5
**File**: `mis/inptt5.f`
**Purpose**: Read input type 5 data (specific bulk data format)
**Implementation Priority**: **HIGH**

---

### 10. **OUTPT5** - Output Type 5
**File**: `mis/outpt5.f`
**Purpose**: Write output type 5 data
**Implementation Priority**: **HIGH**

---

## Database and System Stubs

### 11. **DBASE** - Database Manager
**File**: `mis/dbase.f`
**Purpose**: Main database management interface
**Called By**: Throughout system
**Signature**: Unknown (large routine, ~1,374 lines originally)
**Implementation Priority**: **CRITICAL**

**Note**: Original file excluded due to parsing errors. Needs complete reimplementation or fix of original source.

---

### 12. **NORMAL** - Normalize Vectors
**File**: `mis/normal.f`
**Purpose**: Vector normalization
**Implementation Priority**: **MEDIUM**

---

### 13. **GENPAR** - Generate Parameters
**File**: `mis/genpar.f`
**Purpose**: Generate analysis parameters from input
**Implementation Priority**: **HIGH**

---

### 14. **DESVEL** - Design Velocity
**File**: `mis/desvel.f`
**Purpose**: Compute design velocities for optimization
**Implementation Priority**: **LOW** - Optimization feature

---

### 15. **PROLAT** - Project Latitude
**File**: `mis/prolat.f`
**Purpose**: Geometric projection calculations
**Implementation Priority**: **LOW**

---

### 16. **COMUGV** - Common UGV
**File**: `mis/comugv.f`
**Purpose**: UGV (User Grid Vector?) common block management
**Implementation Priority**: **MEDIUM**

---

### 17. **GFSMA** - GFS Matrix A
**File**: `mis/gfsma.f`
**Purpose**: GFS (Grid Force Summary?) matrix operations
**Implementation Priority**: **MEDIUM**

---

## Parser Error Exclusions (Files with Parsing Issues)

These files had FORTRAN syntax errors during compilation and were excluded. Stub replacements were created.

### 18-21. **ASCM01, ASCM05, ASCM06, ASCM07** - Assembled Spacecraft Models
**Files**: `mis/ascm01.f`, `mis/ascm05.f`, `mis/ascm06.f`, `mis/ascm07.f`
**Purpose**: Spacecraft assembly module functions
**Original Issue**: DATA statement parsing errors
**Signature Example**:
```fortran
SUBROUTINE ASCM01
  ! Stub implementation - original had parser errors
```

**Implementation Priority**: **MEDIUM** - Required for spacecraft models

**Root Cause**: Original files used complex DATA statements with BOZ constants and slash pairs that the parser couldn't handle:
```fortran
DATA  / /  ! Empty slash pair causes "Syntax error in DATA statement"
DATA MASK / 'F00'X /  ! BOZ constant format issues
```

**Required Action**:
1. Fix original source files by updating DATA statements to modern Fortran
2. Or rewrite from scratch based on NASTRAN documentation

---

## Element Shape Function Stubs

### 22. **Q8SHPD** - QUAD8 Shape Functions (Double Precision)
**File**: `mis/q8shpd.f`
**Purpose**: 8-node quadrilateral element shape functions
**Implementation Priority**: **MEDIUM** - Required for QUAD8 elements

---

### 23. **T6SHPD** - TRIA6 Shape Functions (Double Precision)
**File**: `mis/t6shpd.f`
**Purpose**: 6-node triangular element shape functions
**Implementation Priority**: **MEDIUM** - Required for TRIA6 elements

---

### 24. **JACOBD** - Jacobian Matrix (Double Precision)
**File**: `mis/jacobd.f`
**Purpose**: Compute Jacobian matrix for element transformations
**Implementation Priority**: **HIGH** - Core element functionality

---

## Block Data Initialization Stubs

### 25. **FERFBD** - FERF Block Data
**File**: `mis/ferfbd.f`
**Purpose**: Block data initialization for FERF module
**Implementation Priority**: **MEDIUM**

**Note**: May be duplicate of src/blockdata/init/ferfbd.f - verify before implementing

---

## System Call Modifications (Not True Stubs)

### LINK Intrinsic Calls (Disabled)
**Files Modified**:
- `src/system/database/endsys.f` (line 263)
- `mis/pexit.f` (line 46)

**Issue**: Modern gfortran LINK intrinsic requires CHARACTER arguments, original code passes INTEGER

**Current Status**: Calls commented out with TODO notes

**Example**:
```fortran
C     TODO: LINK intrinsic requires CHARACTER args in modern gfortran
C     Original code: CALL LINK (I,ITAB10(I76),0)
C      CALL LINK (I,ITAB10(I76),0)
      GO TO 350
```

**Implementation Priority**: **LOW** - System-specific calls, may not be needed on Linux

---

### RENAME Intrinsic Call (Disabled)
**File Modified**: `src/utilities/output/sofut.f` (line 112)

**Issue**: gfortran RENAME only takes 2 arguments, original code uses 5

**Current Status**: Call commented out with TODO

**Example**:
```fortran
C     TODO: RENAME intrinsic in gfortran only takes 2 args (oldpath, newpath)
C     Original code: CALL RENAME (NAME1,NAME2,IZ(1),NZ,ITEST)
  200 ITEST = 0
C      CALL RENAME (NAME1,NAME2,IZ(1),NZ,ITEST)
      GO TO 50
```

**Implementation Priority**: **MEDIUM** - File operations

---

## Implementation Roadmap

### Phase 1: Critical Functionality (Week 1-2)
**Goal**: Get basic static analysis working

1. **DSSIZE** - Implement sparse matrix size query
2. **GETSTB** / **ENDGTB** - Implement scratch table management
3. **DBASE** - Fix parsing errors and restore original or rewrite
4. **JACOBD** - Implement Jacobian calculations
5. **SDCMPS** - Implement single-precision decomposition

**Estimated Effort**: 40-60 hours

---

### Phase 2: I/O Completion (Week 2-3)
**Goal**: Complete input/output pipeline

1. **INPTT5** / **OUTPT5** - Implement type 5 I/O
2. **FINWRT** - Implement write completion
3. **GENPAR** - Implement parameter generation
4. **TRANSP** - Implement matrix transpose
5. **ONETWO** - Implement precision conversion

**Estimated Effort**: 30-40 hours

---

### Phase 3: Element Support (Week 3-4)
**Goal**: Add higher-order element support

1. **Q8SHPD** - 8-node quad shape functions
2. **T6SHPD** - 6-node triangle shape functions
3. **ASCM01/05/06/07** - Fix parser errors in spacecraft modules

**Estimated Effort**: 20-30 hours

---

### Phase 4: Advanced Features (Week 4-5)
**Goal**: Enable optimization and aeroelastics

1. **APDB** - Aerodynamic panel database
2. **DESVEL** - Design velocities
3. **GFSMA** - GFS matrix operations
4. **COMUGV** - UGV management
5. **PROLAT** - Projection calculations

**Estimated Effort**: 15-25 hours

---

### Phase 5: System Integration (Week 5-6)
**Goal**: Complete system calls and utilities

1. **LINK** - Create portable wrapper for file linking
2. **RENAME** - Create portable wrapper for file renaming
3. **FERFBD** - Complete block data initialization
4. **NORMAL** - Vector normalization

**Estimated Effort**: 10-15 hours

---

## Testing Strategy

For each stub implementation:

1. **Unit Test**: Create standalone test that exercises function in isolation
2. **Integration Test**: Run demo problem that uses the function
3. **Regression Test**: Compare output against reference results
4. **Performance Test**: Verify no significant slowdown vs original

### Test Cases by Priority

**Phase 1 Tests** (Critical):
- `d01011a.inp` - Simple static analysis (QUAD4 elements)
- `d01021a.inp` - Static analysis with constraints
- `d02011a.inp` - Normal modes analysis

**Phase 2 Tests** (I/O):
- `d03011a.inp` - Buckling analysis (tests parameter generation)
- `d04011a.inp` - Transient response

**Phase 3 Tests** (Elements):
- Test files with QUAD8, TRIA6 elements (TBD - may need to create)

**Phase 4 Tests** (Advanced):
- `d07011a.inp` - Aeroelastic flutter
- Optimization test cases (TBD)

---

## Reference Documentation

### Finding Implementation Clues

1. **Search Original Code**: Many stubs may have partial implementations elsewhere
   ```bash
   grep -ri "SUBROUTINE DSSIZE" src/ mis/ mds/ bd/
   ```

2. **Check NASTRAN Manuals**:
   - NASTRAN Programmer's Manual (1970s)
   - NASTRAN Theoretical Manual
   - COSMIC NASTRAN documentation

3. **Similar Functions**: Look for similar functionality under different names
   - DSSIZE → Look for other sparse matrix utilities
   - GETSTB → Look for memory management routines

4. **Cross-Reference Calls**: See what calls the stub
   ```bash
   grep -r "CALL DSSIZE" src/ mis/
   ```

---

## Known Issues

### Parser Errors (ASCM Files)

**Problem**: Original files use FORTRAN 77 constructs not supported by modern gfortran:

```fortran
! Problem 1: Empty DATA statement slash pairs
DATA  / /

! Problem 2: Complex BOZ constants
DATA MASK / 'F00'X /, SHIFT / 12 /

! Problem 3: Continuation in DATA statements
DATA ARRAY / 1, 2, 3,
     1       4, 5, 6 /
```

**Solutions**:
1. Update DATA statements to Fortran 90+ syntax
2. Use PARAMETER instead of DATA where possible
3. Split complex DATA statements into multiple statements

---

### Memory Management

**Issue**: GETSTB/ENDGTB need to integrate with existing memory manager (DBM system)

**Required Knowledge**:
- DBM memory layout (see `src/system/database/dbmmgr.f`)
- Free pool management
- Alignment requirements

---

### Sparse Matrix Format

**Issue**: DSSIZE needs to understand NASTRAN's sparse matrix storage format

**Required Knowledge**:
- NASTRAN sparse matrix format (column-oriented compressed)
- File record structure
- Header format

---

## Success Metrics

### Phase 1 Complete
- [ ] All Phase 1 stubs implemented
- [ ] Unit tests pass
- [ ] d01011a.inp runs to completion
- [ ] Output matches reference (within numerical tolerance)

### Phase 2 Complete
- [ ] All I/O stubs implemented
- [ ] d03011a.inp and d04011a.inp run successfully
- [ ] Log files generated correctly

### Phase 3 Complete
- [ ] QUAD8 and TRIA6 elements functional
- [ ] ASCM files fixed and compiling
- [ ] Spacecraft model test case runs

### Phase 4 Complete
- [ ] Aeroelastic analysis functional
- [ ] Optimization features working

### Phase 5 Complete
- [ ] All 200+ demo problems run without errors
- [ ] Performance within 2x of original NASTRAN
- [ ] Zero known crashes or hangs

---

## Contact and Support

For questions about stub implementations:
1. Check NASTRAN documentation (see docs/references/)
2. Search original COSMIC NASTRAN source
3. Consult NASTRAN user community forums

---

**Last Updated**: February 9, 2026
**Document Version**: 1.0
**Status**: All stubs documented, implementation roadmap defined
