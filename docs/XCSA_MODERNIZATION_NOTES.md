# XCSA.F Modernization Notes

## Overview

`xcsa.f` is the main executive control deck parser for NASTRAN-95. It's a large and complex file that requires careful modernization.

**File Statistics:**
- **Size:** 1366 lines
- **Complexity:** High (complex control flow, many variables, extensive DATA statements)
- **Priority:** Phase 1 High Priority
- **Status:** In Progress

## Current State

### Already Modern Features
- `USE rust_parser_interface` (line 5)
- `TYPE(ExecControlData)` and `TYPE(CaseControlData)` (lines 7-8)
- `EXTERNAL` declaration for bitwise functions (line 11)

### Needs Modernization

#### 1. Implicit Typing
```fortran
! Current:
IMPLICIT INTEGER (A-Z)
```
This makes EVERY undeclared variable an INTEGER by default. Need to:
- Remove IMPLICIT statement
- Add `implicit none`
- Explicitly declare ALL variables

#### 2. Common Blocks
Currently uses 18 COMMON blocks:
- `/XMSSG/` - message strings
- `/RUSTOK/` - **Replace with `use rust_state_module`**
- `/MACHIN/` - machine parameters
- `/SEM/` - semaphore data
- `/SYSTEM/` - system variables (100 words!)
- `/XECHOX/`, `/XRGDXX/`, `/ALTRXX/`, etc.

**Action:** Phase 1 - Keep other COMMON blocks, only replace /RUSTOK/

#### 3. Character Declarations
```fortran
! Old:
CHARACTER UFM*23,UWM*25,UIM*29,SFM*25
CHARACTER*512 RUST_INPFILE

! New:
character(len=23) :: ufm
character(len=25) :: uwm
character(len=29) :: uim
character(len=25) :: sfm
character(len=512) :: rust_inpfile
```

#### 4. Array Declarations
Currently use DIMENSION statement:
```fortran
! Old:
DIMENSION ALTER(2),APPTYP(4),BGNAL(2),...

! New:
integer(int32) :: alter(2), apptyp(4), bgnal(2)
```

## Variables Analysis

### Explicitly Declared Variables (Character/Logical/External)
- `ufm, uwm, uim, sfm` (CHARACTER)
- `rust_inpfile` (CHARACTER*512)
- `tapbit` (LOGICAL function)
- `lshift, rshift, andf, orf, complf` (EXTERNAL)
- `insert(4), delete(9), altrbs, alnogo` (INTEGER - explicit)
- `altfil, erralt, altopn` (INTEGER - explicit)

### Arrays from DIMENSION Statement
- `alter(2), apptyp(4), bgnal(2), cend(2), diagx(11)`
- `dmapbf(1), ectt(51), endal(2), hdg(19), iptdic(1)`
- `iufile(2), iz(2), nxptdc(2), nxcsa(2), osolu(2)`
- `outcrd(200), solrec(6), solu(12), solnm3(7,11)`
- `solnms(7,31), solnm1(7,10), solnm2(7,10), solnmx(6)`
- `xalt(2), xsys(100)`

### COMMON Block Variables
Over 100 variables from COMMON blocks, all implicitly INTEGER except:
- `/XMSSG/`: ufm, uwm, uim, sfm (CHARACTER)
- `/SYSTEM/`: 100+ integer variables
- `/OUTPUT/`: pghdg1-6 (32 words each)

### Local Variables (Implicit INTEGER)
All other variables used in code are implicitly INTEGER:
- `i, j, jj, jk, k, l, n7, m7, s7, i7` (loop counters)
- `imhere` (error location marker)
- `msgnum` (message number)
- `nogo, xnogo, nogox, nogo1` (error flags)
- `icrdct, iseqno, itop, ibot, ldic, nrlfl` (dictionary indices)
- `drecsz, dmapbs, altrbs, nscr, nwpc` (buffer sizes)
- `apprch, apprec, rstrt, soluf, oldalt, notalt` (control variables)
- `icpflg, irestr, nsubst, newalt, alnogo, erralt, altopn` (flags)
- `time, timew, timex` (timing)
- `isign, mask5, allon, mhibyt, endcd` (bit masks)
- And 50+ more...

## Modernization Strategy

### Phase 1 Approach (Recommended)

Given the complexity, use a **conservative modernization** approach:

1. **Convert to free-form** (.f90 extension)
2. **Replace COMMON /RUSTOK/** with `use rust_state_module`
3. **Keep IMPLICIT INTEGER** for now but add comments
4. **Modernize comments** (C → !)
5. **Fix character declarations** (CHARACTER*n → character(len=n))
6. **Keep everything else** (DIMENSION, EQUIVALENCE, DATA, ASSIGN, computed GO TO)

This minimizes risk while achieving the main goals.

### Phase 2 Approach (Future)

For complete modernization:
1. Remove IMPLICIT, add `implicit none`
2. Explicitly declare all variables with `integer(int32)`
3. Convert DIMENSION to modern array syntax
4. Consider replacing computed GO TO with SELECT CASE where possible
5. Convert remaining COMMON blocks to modules

## Estimated Effort

- **Phase 1 (Conservative):** 2-3 hours
  - Mostly mechanical changes
  - Lower risk of introducing bugs
  - Achieves main modernization goals

- **Phase 2 (Complete):** 1-2 days
  - Need to identify all ~150 variables
  - Ensure all variables properly typed
  - Extensive testing required
  - Higher risk of bugs

## Testing Requirements

After modernization:
1. **Build test:** Must compile without errors
2. **Link test:** Must link with rest of NASTRAN
3. **Runtime test:** Run at least 5 example files
4. **Comparison test:** Compare outputs with original xcsa.f version
5. **MESSAGE 507 test:** Verify Rust bridge integration still works

## Recommendation

**For immediate implementation:** Use Phase 1 (Conservative) approach.

**Rationale:**
1. `semint.f90` is complete and working
2. `xcsa.f` is 7x larger than `semint.f`
3. Conservative approach maintains stability
4. Still achieves key modernization goals:
   - Free-form source
   - Rust bridge module integration
   - Modern comment style
   - Cleaner character declarations

**After Phase 1 completes and is validated**, proceed to Phase 2 for full modernization.

## Next Steps

1. Create `xcsa.f90` using Phase 1 approach
2. Update `src/utilities/CMakeLists.txt` (already done - includes .f90 files)
3. Build and test
4. If successful, mark Phase 1 complete
5. Plan Phase 2 for future work

---

**Created:** 2026-02-10
**Status:** In Progress
**Related Files:**
- [semint.f90](../src/utilities/helpers/semint.f90) - Completed modernization example
- [FORTRAN_MODERNIZATION_PLAN.md](FORTRAN_MODERNIZATION_PLAN.md) - Overall plan
