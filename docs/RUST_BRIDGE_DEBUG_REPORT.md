# Rust Bridge Debug Report
**Date:** 2026-02-10
**Issue:** Rust bridge MESSAGE 507 bypass not working
**Status:** Root cause identified - awaiting resolution

---

## Summary

The Rust parser bridge integration was not working because **the modernized [xcsa.f90](../src/utilities/output/xcsa.f90) code was never executing**. The linker was using the old [xcsa.f](../mis/xcsa.f) from the legacy MIS directory instead.

Even after fixing the build system to exclude the old file, **a deeper execution flow issue remains**: The XCSA initialization section and Rust bridge code sections do not execute, even though the card processing loop does execute (proven by MESSAGE 507 errors appearing).

---

## Investigation Timeline

### Issue #1: Duplicate Symbol Conflict (SOLVED)

**Problem:** Both `mis/xcsa.f` (old) and `src/utilities/output/xcsa.f90` (new) were being compiled and linked.

**Evidence:**
```bash
$ find build -name "*xcsa*" -type f
./src/legacy/CMakeFiles/nastran_legacy.dir/__/__/mis/xcsa.f.o  # OLD
./src/utilities/CMakeFiles/nastran_output.dir/output/xcsa.f90.o  # NEW
```

**Root Cause:**
`src/legacy/CMakeLists.txt` line 10:
```cmake
file(GLOB LEGACY_MIS_SOURCES ${CMAKE_SOURCE_DIR}/mis/*.f)
```
This glob pattern included `mis/xcsa.f`, which was compiled alongside the modernized version.

**Fix Applied:**
Added exclusion to `src/legacy/CMakeLists.txt`:
```cmake
list(REMOVE_ITEM LEGACY_MIS_SOURCES ${CMAKE_SOURCE_DIR}/mis/xcsa.f)
```

**Result:** Old xcsa.f no longer compiled. Only [xcsa.f90](../src/utilities/output/xcsa.f90) is used.

---

### Issue #2: XCSA Code Not Executing (UNSOLVED)

**Problem:** After fixing the duplicate symbol issue, the Rust bridge code still doesn't execute.

**Test Method:** Added debug file creation markers:
- `xcsa.f90:141` - Initialization section marker (`/tmp/xcsa_entry.txt`)
- `xcsa.f90:212` - Rust bridge section marker (`/tmp/rust_bridge_executed.txt`)

**Results:**
```bash
✗ XCSA initialization NOT executed (/tmp/xcsa_entry.txt not created)
✗ Rust bridge section NOT executed (/tmp/rust_bridge_executed.txt not created)
```

**BUT:**
```
✓ MESSAGE 507 errors DO appear (proves XCSA card processing loop IS executing)
```

**Evidence from Output:**
```
0*** USER FATAL MESSAGE  507, ILLEGAL SPECIFICATION OR FORMAT ON PRECEDING CARD.
     IMHERE =  520     ← Generated at xcsa.f90:926 in card loop
```

---

## Code Flow Analysis

### Expected Flow (Not Happening):
```
xcsa (line 1)
  ↓
Initialization (lines 141-176) ✗ NOT EXECUTING
  ↓
call page (line 205)
  ↓
Rust Bridge Code (lines 207-248) ✗ NOT EXECUTING
  ↓
Card Processing Loop (label 20, line 278) ✓ EXECUTING
  ↓
MESSAGE 507 Handler (line 760+) ✓ EXECUTING
```

### Actual Flow (Mystery):
```
xcsa (line 1)
  ↓
??? Unknown Jump ???
  ↓
Card Processing Loop (label 20, line 278) ✓ EXECUTING
```

---

## Verification Tests

### Test 1: Object File Content
```bash
$ strings build/src/utilities/.../xcsa.f90.o | grep -i "executed"
/tmp/xcsa_entry.txt
XCSA SUBROUTINE ENTERED
/tmp/rust_bridge_executed.txt
Rust bridge code section WAS executed
```
**Result:** Debug code IS compiled into object file ✓

### Test 2: Executable Symbol
```bash
$ nm build/bin/nastran | grep xcsa_
0000000000786928 T xcsa_
```
**Result:** Only ONE xcsa_ symbol in executable (from new xcsa.f90) ✓

### Test 3: Runtime Execution
```bash
$ ./build/bin/nastran d01001a.inp
$ ls /tmp/xcsa_entry.txt /tmp/rust_bridge_executed.txt
ls: cannot access: No such file or directory
```
**Result:** Debug markers NOT created - code not executing ✗

### Test 4: Output Log Check
```
1. NASTRAN banner appears ✓
2. Executive Control Deck Echo appears ✓
3. MESSAGE 507 errors appear ✓
4. No Rust bridge debug output ✗
```

---

## Hypotheses

### Hypothesis 1: COMMON Block Initialization Semantics
Fortran COMMON blocks with DATA statements may cause the initialization section to be skipped. The compiler might be optimizing away the initialization code if variables are already initialized via COMMON/DATA.

### Hypothesis 2: Hidden ENTRY Point
There may be an undocumented ENTRY point in XCSA that allows direct entry to the card processing loop, bypassing initialization.

### Hypothesis 3: SAVE Attribute Behavior
Variables in COMMON blocks have implicit SAVE attribute. The subroutine might detect "already initialized" state and skip the initialization section on first call.

### Hypothesis 4: Compiler Optimization
Aggressive compiler optimizations might be removing "dead code" if the initialization appears to have no side effects on first analysis pass.

---

## Attempted Fixes

1. ✅ Excluded old xcsa.f from legacy build
2. ✅ Changed WRITE from unit 3 to OUTTAP
3. ✅ Added file creation markers (OPEN statements)
4. ❌ Tried PRINT statements (Fortran ordering rules prevent this)
5. ❌ Code still doesn't execute

---

## Current Status

- **Phase 1 Fortran modernization:** ✅ Complete (semint.f90, xcsa.f90)
- **Build system fix:** ✅ Complete (old xcsa.f excluded)
- **Rust bridge integration:** ❌ Not working (code path unreachable)

---

## Next Steps

### Option A: Investigate XCSA Control Flow (Recommended)
1. Disassemble the xcsa.f90.o object file to examine actual machine code
2. Check if initialization code exists in compiled output
3. Trace execution path to understand why it's skipped
4. Look for compiler pragmas or directives that might affect control flow

### Option B: Alternative Integration Point
1. Move Rust bridge to SEMINT (guaranteed to execute)
2. Set IRUST_OK flag in SEMINT before XCSA is called
3. XCSA MESSAGE 507 handler checks flag (already in place)
4. Bypasses the non-executing code sections

### Option C: Legacy xcsa.f Integration
1. Modernize the original mis/xcsa.f instead of the copy
2. Ensure the "correct" version is being used
3. May reveal why code paths differ

---

## Files Modified

| File | Status | Notes |
|------|--------|-------|
| [src/utilities/output/xcsa.f90](../src/utilities/output/xcsa.f90) | Modified | Phase 1 modernization + Rust bridge |
| [src/utilities/helpers/semint.f90](../src/utilities/helpers/semint.f90) | Modified | Full F2008/2018 modernization |
| [src/utilities/helpers/rust_state_module.f90](../src/utilities/helpers/rust_state_module.f90) | Created | Modern module for IRUST_OK flag |
| [src/legacy/CMakeLists.txt](../src/legacy/CMakeLists.txt) | Modified | Exclude mis/xcsa.f |
| [docs/nastran_execution_flow.svg](nastran_execution_flow.svg) | Created | Execution flowchart |

---

## Conclusion

While the build system issue has been resolved, a deeper control flow mystery remains. The modernized XCSA code compiles correctly and is linked into the executable, but the initialization and Rust bridge sections do not execute at runtime despite being present in the object code.

The most practical solution is **Option B**: Move the Rust bridge to SEMINT, which is proven to execute and already calls XCSA. This would bypass the non-executing sections while still achieving the MESSAGE 507 bypass functionality.

---

**For visualization, see:** [nastran_execution_flow.svg](nastran_execution_flow.svg)
