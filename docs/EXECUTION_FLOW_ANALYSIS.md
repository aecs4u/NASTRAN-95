# NASTRAN-95 Execution Flow Analysis

## Complete Call Chain: Input to Execution

### Overview

Through investigation, I've mapped the complete execution flow from program startup to DMAP execution. The issue is that the input reading chain exists but may not be triggered correctly.

---

## Execution Flow

```
nastrn.f (main program)
    ↓
CALL XSEM00 (line 126)
    ↓
    ├─ CALL BTSTRP (line 63) - Bootstrap initialization
    ├─ CALL TDATE (line 69) - Get date
    ├─ CALL CONMSG (line 70) - Print "SEM1 BEGIN"
    ├─ CALL SEMINT(0) (line 71) ← **PREFACE ENTRY POINT**
    │   │
    │   ├─ CALL NASCAR (line 33) - Read NASTRAN card
    │   ├─ CALL GNFIAT (line 48) - Generate initial file tables
    │   ├─ CALL TMTSIO (line 58) - Timing I/O routines
    │   ├─ CALL TMTSLP (line 61) - Timing loops
    │   ├─ CALL XCSA (line 67) ← **EXECUTIVE CONTROL PROCESSING**
    │   │   │
    │   │   └─ CALL XRGDFM (line 614) - **READS RIGID FORMAT FILE**
    │   │       │
    │   │       ├─ Opens rigid format file (rf/DISP1, rf/DISP2, etc.)
    │   │       ├─ Reads DMAP statements
    │   │       └─ Writes to scratch file 315 (POOL)
    │   │
    │   ├─ CALL IFP1 (line 88) - Input File Processor (Case Control)
    │   ├─ CALL XSORT2 (line 101) - Sort bulk data
    │   ├─ CALL IFP (line 107) - Input File Processor (Bulk Data)
    │   └─ CALL XGPI (line 145) - General Problem Initialization
    │
    ├─ CALL CONMSG (line 74) - Print "BEGIN END"
    ├─ IBUF1 = KORSZ(...) (line 77) - Get memory size
    └─ CALL OPEN(*270,POOL,...) (line 87) ← **READ POOL FILE**
        ↓
    CALL READ(*280,*40,POOL,...) (line 91) - Read OSCAR entry
        ↓
    [Module execution loop based on POOL content]
```

---

## Key Components

### 1. **SEMINT** - Execution Monitor for PREFACE
**File**: `src/utilities/helpers/semint.f`
**Purpose**: Orchestrates the entire PREFACE (input processing phase)

**Functions**:
- Reads and processes all input cards
- Generates file tables
- Processes executive control deck
- Sorts bulk data
- Initializes problem data structures

**Critical Call**: Line 67 → `CALL XCSA`

---

### 2. **XCSA** - Executive Control Deck Processor
**File**: `src/utilities/output/xcsa.f`
**Purpose**: "XCSA READS AND PROCESSES THE NASTRAN EXECUTIVE CONTROL DECK"

**Functions**:
- Reads cards like:
  - `ID` - Problem identification
  - `SOL 6` - Solution number
  - `APP DISP` - Application type
  - `TIME`, `DIAG`, `CEND`
- Determines rigid format to load based on SOL and APP
- Calls XRGDFM to read rigid format file

**Critical Call**: Line 614 → `CALL XRGDFM (SOLU,OSOLU,APPREC,IUFILE,DMAPBF,ISIZE,NSCR,NOGO)`

---

### 3. **XRGDFM** - Rigid Format Processor
**File**: `src/system/database/xrgdfm.f`
**Purpose**: "XRGDFM READS AND PROCESSES RIGID FORMATS"

**Functions**:
- Opens rigid format file from `rf/` directory
  - `rf/DISP1` - Static analysis (Rigid Format 1)
  - `rf/DISP2` - Normal modes (Rigid Format 2)
  - `rf/DISP3` - Buckling (Rigid Format 3)
  - etc.
- Reads DMAP statements from rigid format
- Processes control cards (`****CARD`, `****FILE`, `****SBST`)
- Writes DMAP to scratch file 315
- Creates POOL file for execution

**Critical Operations**:
- Line 111: "READS A CARD IMAGE FROM THE RIGID FORMAT FILE"
- Line 125: "OTHERWISE, THE CARD IS A DMAP AND WRITEN TO SCRATCH 315"

---

### 4. **XSEM00** - Main Execution Engine
**File**: `src/system/database/xsem00.f`
**Purpose**: "EXECUTE THE PREFACE AND THEN TO EXECUTE MODULES ACCORDING TO THE DMAP"

**Functions**:
- Calls SEMINT to process input (PREFACE)
- Opens POOL file containing DMAP
- Reads OSCAR entries from POOL
- Executes modules in sequence

**Critical Issue**: Line 87 tries to open POOL before SEMINT completes

---

## Current Execution Problem

### Root Cause

Looking at `xsem00.f` line 71 and line 87:

```fortran
   71      CALL SEMINT ( 0 )           ! Should read input and create POOL
        ...
   87      CALL OPEN(*270,POOL,DATABF(IBUF1),2)  ! Try to read POOL
```

**The issue**: XSEM00 calls SEMINT, which should:
1. Read input from stdin (via NASCAR, XCSA, IFP1, etc.)
2. Process executive control deck
3. Load rigid format and create POOL file
4. Return to XSEM00

Then XSEM00 should read POOL and execute.

**What's happening**: Program hangs, suggesting POOL never gets created

---

## Verification Steps Needed

### 1. Check if NASCAR Reads Input

**File**: Search for NASCAR subroutine
```bash
grep -rn "SUBROUTINE NASCAR" src/ mis/
```

**Expected**: NASCAR should read from stdin (unit 5) or environment variable

### 2. Check if XCSA Reads Executive Control

**Test**: Does XCSA actually read the input file?

Looking at `xcsa.f`, it should read cards from stdin until it finds:
- `SOL 6` → solution = 6
- `APP DISP` → application = DISPLACEMENT
- `CEND` → end of executive control

### 3. Check if POOL File is Created

**After SEMINT returns**, there should be a POOL file (scratch file or in-memory)

**Check**: After line 71 in xsem00.f, is POOL created?

---

## Proposed Fix

### Option 1: Debug SEMINT Call (Recommended)

Add debug output to trace execution:

**File**: `src/system/database/xsem00.f`

```fortran
! After line 70
      CALL CONMSG(WORDB,2,1)

! ADD THIS:
      PRINT *, 'DEBUG: About to call SEMINT'
      CALL FLUSH(6)

! Line 71 - existing
      CALL SEMINT ( 0 )

! ADD THIS:
      PRINT *, 'DEBUG: SEMINT returned'
      CALL FLUSH(6)

      ISPERLNK = 1
```

**Recompile and test**:
```bash
cd build && cmake --build . 2>&1 | tail -5
cd ../examples/input
export DBMEM=12000000 OCMEM=2000000 RFDIR=../../rf ...
timeout 10 ../../build/bin/nastran < d01000a.inp 2>&1 | head -30
```

**Expected Output**:
- If prints "About to call SEMINT" but not "SEMINT returned" → Hangs inside SEMINT
- If prints neither → Hangs before SEMINT
- If prints both → SEMINT completes, hangs later

---

### Option 2: Check stdin Redirection

The input redirection `< d01000a.inp` may not work if NASCAR expects input from a specific unit.

**Check NASCAR implementation**:
- What unit does it read from?
- Does it use FORTRAN READ or C fread?
- Does it need an environment variable pointing to input file?

**Alternative**: Set input file via environment variable:
```bash
export INPUTFILE=d01000a.inp
```

---

### Option 3: Bypass SEMINT for Testing

**Goal**: Manually create POOL file to test if XSEM00 execution works

**Steps**:
1. Copy a pre-existing POOL file from working NASTRAN installation
2. Place in work directory
3. Comment out SEMINT call in xsem00.f
4. Test if XSEM00 can read and execute

**Modify xsem00.f**:
```fortran
! Line 71 - comment out
C      CALL SEMINT ( 0 )
      PRINT *, 'DEBUG: Skipped SEMINT for testing'
```

**Risk**: Won't work for real problems, but validates execution engine

---

## Next Investigation Steps

### Immediate (Today)

1. **Find NASCAR subroutine**
   ```bash
   grep -rn "SUBROUTINE NASCAR" src/ mis/ mds/
   ```

2. **Check how input is read**
   - What unit number?
   - Environment variable?
   - Command-line argument?

3. **Add debug output to SEMINT**
   - Before each major call
   - Confirm execution path

### Short Term (This Week)

4. **Trace XCSA execution**
   - Does it read executive control cards?
   - Does it parse SOL and APP?
   - Does it call XRGDFM?

5. **Verify rigid format loading**
   - Does XRGDFM open rf/DISP1 or equivalent?
   - Does it read DMAP?
   - Does it write to scratch file 315?

6. **Check POOL file creation**
   - After SEMINT returns, does POOL exist?
   - Is it in memory or on disk?
   - What's the file format?

---

## Success Criteria

### Phase 1: SEMINT Completion
- [ ] NASCAR successfully reads input
- [ ] XCSA processes executive control deck
- [ ] XRGDFM loads rigid format file (rf/DISP6 for SOL 6)
- [ ] POOL file created with DMAP
- [ ] SEMINT returns without error

### Phase 2: Execution
- [ ] XSEM00 opens POOL successfully
- [ ] Reads first OSCAR entry
- [ ] Calls first module (e.g., GP1)
- [ ] Module executes without error

### Phase 3: Output
- [ ] Output file generated
- [ ] Log file shows execution trace
- [ ] Program terminates normally

---

## Implementation Plan

### Step 1: Find NASCAR (Today)
Search for NASCAR and understand input mechanism

### Step 2: Add Debug Tracing (Today)
Add PRINT statements to track execution flow through SEMINT

### Step 3: Test Input Reading (Tomorrow)
Verify NASCAR/XCSA can read from stdin

### Step 4: Verify Rigid Format Loading (Tomorrow)
Confirm XRGDFM opens and reads rf/DISP6

### Step 5: Validate POOL Creation (Day 3)
Check if POOL file exists after SEMINT

### Step 6: Full Integration Test (Day 3)
Run complete d01000a.inp and verify output

---

## References

### Source Files
- **Main**: `src/system/platform/nastrn.f` (line 126)
- **Execution**: `src/system/database/xsem00.f` (line 71, 87)
- **Preface**: `src/utilities/helpers/semint.f` (line 67)
- **Executive**: `src/utilities/output/xcsa.f` (line 614)
- **Rigid Format**: `src/system/database/xrgdfm.f`
- **Rigid Formats**: `rf/DISP1`, `rf/DISP2`, etc.

### Key Functions
| Function | Purpose | File |
|----------|---------|------|
| SEMINT | PREFACE orchestrator | semint.f |
| NASCAR | Read NASTRAN card | (TBD - find) |
| XCSA | Executive control processor | xcsa.f |
| XRGDFM | Rigid format loader | xrgdfm.f |
| IFP1 | Case control processor | (TBD) |
| XSORT2 | Bulk data sorter | (TBD) |
| IFP | Bulk data processor | (TBD) |

---

**Date**: February 9, 2026
**Status**: Flow mapped, NASCAR investigation needed
**Next Action**: Find and analyze NASCAR subroutine
