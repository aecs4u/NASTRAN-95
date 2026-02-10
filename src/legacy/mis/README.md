# Legacy MIS Files Archive

This directory contains **archived** versions of files that have been modernized and moved to [src/utilities/](../../utilities/).

These files are **NOT compiled** into the NASTRAN executable. They are kept for reference only.

---

## Archived Files

| Legacy File | Modernized Version | Date | Status |
|-------------|-------------------|------|--------|
| [xcsa.f](xcsa.f) | [src/utilities/output/xcsa.f90](../../utilities/output/xcsa.f90) | 2026-02-10 | ✅ Phase 1 complete |
| [semint.f](semint.f) | [src/utilities/helpers/semint.f90](../../utilities/helpers/semint.f90) | 2026-02-10 | ✅ Phase 2 complete |
| [ffread.f](ffread.f) | [src/utilities/parser/ffread.f90](../../utilities/parser/ffread.f90) | 2026-02-10 | ✅ Moved to utilities |
| [finwrt.f](finwrt.f) | (ENTRY in onetwo.f) | 2026-02-10 | ✅ Stub removed (conflict) |

---

## Modernization Details

### xcsa.f → xcsa.f90 (Phase 1 - Conservative)
**Executive Control Deck Parser**

**Changes:**
- ✅ Converted to free-form Fortran (.f90)
- ✅ Replaced `COMMON /RUSTOK/` with `use rust_state_module`
- ✅ Modernized comments (C → !)
- ✅ Fixed character declarations (`CHARACTER*n` → `character(len=n)`)
- ✅ Fixed rank mismatches (ZERO array, timex/timew arrays)
- ✅ Fixed Hollerith constants case (restored UPPERCASE)
- ✅ Added Rust bridge integration (lines 207-248)
- ✅ Added MESSAGE 507 bypass logic (line 1204)

**Preserved for Phase 2:**
- `IMPLICIT INTEGER (A-Z)`
- DIMENSION statements
- EQUIVALENCE statements
- DATA statements
- Computed GO TO
- ASSIGN statements

**Result:** Fully functional, passes d01001a test

---

### semint.f → semint.f90 (Phase 2 - Complete)
**Preface Execution Monitor**

**Changes:**
- ✅ Converted to free-form Fortran (.f90)
- ✅ Full F2008/2018 modernization
- ✅ Removed IMPLICIT, added `implicit none`
- ✅ Explicit variable declarations with `integer(int32)`
- ✅ Added `intent(in)`, `intent(out)` specifications
- ✅ Modernized comments and structure
- ✅ Used `iso_fortran_env` module

**Result:** Fully modernized, production ready

---

### ffread.f → ffread.f90
**File Reader**

**Status:** Moved to utilities, Phase 1 modernization

---

### finwrt.f (Stub - Removed)
**Conflict Resolution**

**Issue:** `onetwo.f` contains `ENTRY FINWRT`, creating a symbol conflict with standalone `finwrt.f` stub.

**Resolution:** Moved stub to archive. Real implementation is ENTRY point in `onetwo.f`.

---

## Critical Lessons

### 1. Hollerith Constants Are Case-Sensitive

❌ **WRONG:**
```fortran
data ectt / 51, &
     4htime,4h    ,0   ,   4happ ,4h    ,0
```

✅ **CORRECT:**
```fortran
data ectt / 51, &
     4HTIME,4H    ,0   ,   4HAPP ,4H    ,0
```

**Why:** The `H` prefix is case-insensitive, but the CHARACTER DATA must match input cards exactly. Lowercase breaks card matching.

### 2. Duplicate Symbols

When migrating files, the old version in `mis/` must be moved to `src/legacy/mis/`. The build system (`src/legacy/CMakeLists.txt`) auto-excludes files that exist in `src/utilities/`.

**Verification:**
```bash
nm --print-size build/bin/nastran | grep "symbol_"
nm --print-size build/src/utilities/.../file.f90.o | grep "symbol_"
# Sizes should MATCH
```

### 3. ENTRY Point Conflicts

Some files contain `ENTRY` statements defining additional symbols:
- `onetwo.f` → `ENTRY FINWRT`
- Both the host file AND standalone file must be handled

---

## Build System Integration

### Auto-Exclusion Logic
`src/legacy/CMakeLists.txt` automatically excludes any `mis/*.f` file that has a corresponding file in `src/utilities/`:

```cmake
foreach(UTIL_FILE ${UTIL_ALL_SOURCES})
  get_filename_component(BASENAME ${UTIL_FILE} NAME_WE)
  set(LEGACY_FILE ${CMAKE_SOURCE_DIR}/mis/${BASENAME}.f)
  if(LEGACY_FILE IN_LIST LEGACY_MIS_SOURCES)
    list(REMOVE_ITEM LEGACY_MIS_SOURCES ${LEGACY_FILE})
    math(EXPR EXCLUDED_COUNT "${EXCLUDED_COUNT} + 1")
  endif()
endforeach()
```

**Result:** 304 files auto-excluded from legacy build (2026-02-10).

---

## Migration Process

For new file migrations, see: [Migration Checklist](../../../docs/MIGRATION_CHECKLIST.md)

**Quick steps:**
1. Copy `mis/file.f` to `src/utilities/{subdir}/file.f90`
2. Modernize (preserve Hollerith case!)
3. Move `mis/file.f` to `src/legacy/mis/file.f`
4. Build and verify symbols match
5. Update this README

---

## References

- [XCSA Fix Complete Report](../../../docs/XCSA_FIX_COMPLETE.md)
- [Migration Checklist](../../../docs/MIGRATION_CHECKLIST.md)
- [Fortran Modernization Plan](../../../docs/FORTRAN_MODERNIZATION_PLAN.md)

---

**Last Updated:** 2026-02-10
**Total Files Archived:** 4
**Excluded from Build:** 304+ (auto-detected duplicates)
