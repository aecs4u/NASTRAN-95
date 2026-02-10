# NASTRAN-95 Modernization - Build & Runtime Status

**Date:** February 10, 2026
**Build System:** CMake 3.30, GFortran 13.3.0
**Platform:** Linux x86-64

---

## ✅ Successfully Completed

### Build System
- **Compilation**: All 2,500+ Fortran source files compile successfully
- **Executable**: 8.7 MB binary created at `build/bin/nastran`
- **Libraries**: Intel MKL 2025.3 linked for optimized BLAS/LAPACK
- **Parallelization**: OpenMP enabled for multi-threaded element processing
- **Build Time**: < 2 minutes with parallel compilation (vs 2-3 hours original)

### Runtime Initialization
- **Memory Management**: Database and open core allocation working
- **File I/O**: GINO I/O system operational
- **PREFACE Module**: Located (`SEMINT`) and functional
- **Banner Output**: Correctly displays NASTRAN header

### Performance Optimization
- **GINO Timing Bypass**: Configured via `/rf/NASINFO`
  - Set `TIM = 16` with pre-computed timing constants
  - Eliminates 5-30 minute calibration delay on first run
  - File: `rf/NASINFO` lines 127-129

### Environment Setup
Required environment variables documented in `scratch/run_nastran.sh`:
```bash
export DBMEM=12000000      # Database memory (words)
export OCMEM=2000000       # Open core memory
export RFDIR=/tmp/nrf      # Rigid format directory (short path required)
export DIRCTY=/tmp         # Scratch directory
export LOGNM=*.log         # Log file
export DICTNM=*.dic        # Dictionary file
export FTN11-FTN21=none    # Optional Fortran units
export SOF1-SOF10=none     # SOF files
```

---

## ❌ Known Issues

### Critical: Input Card Parser Bug

**Symptom**: Executive and Case Control cards are rejected with format errors

**Affected Cards**:
- `DIAG 14,20` → MESSAGE 507: "ILLEGAL SPECIFICATION OR FORMAT"
- `DISP = ALL` → MESSAGE 603: "CARD DOES NOT END PROPERLY"
- `ENDDATA` → Reads as 'ENDDATA,' (extra comma added)

**Root Cause**: Unknown parsing issue in card reading routines
- Not related to line endings (CRLF vs LF)
- Not related to file format (80-column cards correct)
- Likely Fortran string handling or character encoding issue in modernized build

**Files Involved**:
- `mis/ffread.f` - Free-field card reader (error at line 267)
- `src/utilities/parser/xread.f` - Card processor
- `src/utilities/output/xcsa.f` - Executive control processor (IMHERE=520)

**Impact**: **BLOCKS ALL ANALYSIS EXECUTION**
- Input parsing fails
- DMAP compilation cannot proceed
- No analyses can run

---

## 📊 Comparison with Reference

| Component | 1995 Original | 2026 Modernized |
|-----------|---------------|-----------------|
| Build System | Make | CMake ✓ |
| Compile Time | 2-3 hours | < 2 minutes ✓ |
| Executable Size | ~8 MB | 8.7 MB ✓ |
| Initialization | Works | Works ✓ |
| Timing Calibration | 5-30 min | Bypassed ✓ |
| Input Parsing | Works | **BROKEN** ❌ |
| Analysis Execution | Works | Blocked ❌ |

---

## 🔍 Debugging Attempted

1. ✅ Added verbose debug output to initialization (`nastrn.f`, `xsem00.f`)
2. ✅ Configured NASINFO to skip timing calibration
3. ✅ Created symbolic link for short RFDIR path
4. ✅ Tested both CRLF and LF line endings
5. ✅ Added debug output to XREAD parser
6. ❌ Card parsing bug remains unresolved

**Next Steps for Resolution**:
- Deep dive into KHRBCD (character-to-BCD conversion) routine
- Compare gfortran character handling vs original compilers
- Add byte-level debug output to ffread.f card reading
- Test with original unmodified source (mis/ directory)
- Check if Fortran string length declarations changed during migration

---

## 📁 Test Results

### Test Case: d01000a.inp
**Description**: Print DMAP rigid format listing (minimal test)
**Expected**: 458-line output with DMAP compiler listing
**Actual**: 107-line output, terminates with card errors

**Output Comparison**:
```
Reference:  458 lines, complete DMAP listing
Our build:  107 lines, stops at card parsing errors
```

**Files**:
- Input: `examples/input/d01000a.inp`
- Reference: `examples/reference_output/d01000a.out`
- Actual: `scratch/d01000a_unix.out`
- Debug: `scratch/debug_unix.log`

---

## 🚀 How to Build

```bash
cd /mnt/developer/git/aecs4u.it/NASTRAN-95/build
cmake -DBLA_VENDOR=Intel10_64lp \
      -DCMAKE_PREFIX_PATH=/opt/intel/oneapi/mkl/2025.3 ..
make -j$(nproc)
```

**Output**: `build/bin/nastran` (8.7 MB)

---

## 🔧 Configuration Files Modified

1. **rf/NASINFO** (lines 127-129)
   - Changed `TIM = -99` to `TIM = 16`
   - Added timing constants to bypass calibration

2. **src/system/platform/nastrn.f**
   - Added verbose debug output for initialization tracking

3. **src/system/database/xsem00.f**
   - Added debug output for PREFACE execution flow

4. **src/utilities/parser/xread.f**
   - Added card input debug output (not yet effective)

---

## 📝 Conclusion

The NASTRAN-95 modernization has successfully:
- ✅ Modernized the build system from Make to CMake
- ✅ Achieved dramatic build time improvements (80x faster)
- ✅ Successfully initializes and runs to input parsing stage
- ✅ Bypassed performance bottlenecks (timing calibration)

**However**, a critical bug in the input card parser prevents execution of any analyses. This appears to be related to Fortran string handling differences between the 1995 compilers and modern gfortran, and requires additional debugging to resolve.

The infrastructure is in place for a working NASTRAN-95, but the card parser needs to be fixed before the system can execute analyses.
