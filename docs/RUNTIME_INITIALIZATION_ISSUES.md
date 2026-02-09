# NASTRAN-95 Runtime Initialization Issues

## Executive Summary

The NASTRAN-95 executable compiles successfully (8.7 MB, ELF 64-bit) but encounters runtime initialization problems that prevent analysis execution. This document analyzes the issues and proposes solutions.

**Current Status**: ❌ **BLOCKED** - Executable hangs during initialization
**Root Cause**: Missing POOL/OSCAR data files and incomplete database setup
**Impact**: Cannot run any analysis problems
**Priority**: **CRITICAL** - Prevents all testing and usage

---

## Test Configuration

### Test Case
**File**: `examples/input/d01000a.inp`
**Description**: Minimal test - prints rigid format DMAP listing
**Size**: 1.7 KB, 21 lines
**Expected Behavior**: Print DMAP listing and exit
**Actual Behavior**: Hangs indefinitely, produces no output

### Environment Setup
```bash
export DBMEM=12000000      # Database memory (words)
export OCMEM=2000000       # Open core memory (words)
export RFDIR=/path/to/rf   # Rigid format directory
export DIRCTY=/tmp         # Scratch directory
export LOGNM=test.log      # Log file
export PLTNM=test.plt      # Plot file
export DICTNM=test.dic     # Dictionary file
export PUNCHNM=test.pun    # Punch file
export OPTPNM=none         # Optional files
export NPTPNM=none
export FTN11=none
export FTN12=none
# ... (FTN13-21, SOF1-10)
```

### Execution
```bash
cd examples/input
/path/to/nastran < d01000a.inp > output.txt 2>&1
```

**Result**: Timeout after 10 seconds, no output produced

---

## Error Analysis

### Initial Error (No Environment Variables)

**Error Message**:
```
At line 34 of file src/system/platform/nastrn.f
Fortran runtime error: End of file

Error termination. Backtrace:
#0  0x716bd2623e59 in ???
#1  0x716bd2624a71 in ???
...
```

**Root Cause**:
- Line 34 in nastrn.f: `READ ( VALUE, * ) IDBLEN`
- Attempts to read DBMEM environment variable into VALUE
- VALUE is empty string when environment variable not set
- READ from empty string causes "End of file" error

**Solution**: Set DBMEM and OCMEM environment variables ✅

---

### Current Error (With Environment Variables)

**Symptom**:
- Program hangs indefinitely
- No output to stdout or log file
- CPU usage at 0% (not looping, blocking on I/O)

**Analysis**:

#### Program Flow (nastrn.f)

```fortran
PROGRAM NASTRN
  ! Lines 22-26: Initialize timing
  CALL SECOND (SYSTM(18))
  CALL WALTIM (ISYSTM(32))

  ! Lines 32-36: Read memory configuration
  CALL BTSTRP
  CALL GETENV ( 'DBMEM', VALUE )
  READ ( VALUE, * ) IDBLEN    ! ✅ Works now
  CALL GETENV ( 'OCMEM', VALUE )
  READ ( VALUE, * ) IOCMEM    ! ✅ Works

  ! Lines 40-43: Set up memory
  IDBLEN = LENOPC - IOCMEM
  LASTAD = LOCFX( IZ( IOCMEM ) )
  IDBADR = LOCFX( IZ( IOCMEM+1 ) )

  ! Line 44: Initialize database manager
  CALL DBMINT                  ! ✅ Completes

  ! Lines 50-114: Set up file paths from environment
  CALL GETENV ( 'RFDIR',  RFDIR  )    ! ✅
  CALL GETENV ( 'DIRCTY', DIRTRY )    ! ✅
  ! ... [many GETENV calls] ...

  ! Lines 115-125: Open files
  OPEN ( 3, FILE=DSNAMES(3), STATUS='UNKNOWN')  ! Log file ✅
  OPEN (11, FILE=DSNAMES(11), STATUS='UNKNOWN') ! if not 'none' ✅
  ! ...

  ! Line 126: Execute main NASTRAN engine
  CALL XSEM00                  ! ❌ HANGS HERE

  STOP
END PROGRAM
```

#### XSEM00 Main Engine (xsem00.f)

```fortran
SUBROUTINE XSEM00
  ! Line 63: Bootstrap again
  CALL BTSTRP

  ! Line 69: Get date
  CALL TDATE(XX(14))

  ! Line 70: Print message
  CALL CONMSG(WORDB,2,1)      ! Should print "SEM1 BEGIN"

  ! Line 71: Initialize SEM
  CALL SEMINT ( 0 )

  ! Line 74: Print message
  CALL CONMSG ( WORDB,2,1)    ! Should print "BEGIN END"

  ! Line 77: Get memory size
  IBUF1 = KORSZ(DATABF)-SYSBUF

  ! Line 87: OPEN POOL FILE
  CALL OPEN(*270,POOL,DATABF(IBUF1),2)  ! ❌ LIKELY HANGS HERE

  ! Line 91: READ OSCAR ENTRY
30  CALL READ(*280,*40,POOL,INOSCR,200,1,ERRFLG)  ! ❌ OR HERE

  ! ... [rest of execution loop] ...
```

---

## Root Cause: Missing POOL File

### What is POOL?

**POOL** is the OSCAR (Output SChedule And Run) file that contains:
- DMAP (Direct Matrix Abstraction Program) instructions
- Module execution sequence
- Data flow between modules
- Control parameters

**POOL Format**:
- Binary file created by NASTRAN preface
- Contains compiled DMAP from:
  1. Rigid Format files (rf/rigid*.f)
  2. User DMAP modifications
  3. Solution sequence parameters

**Location**: In-memory temporary file or scratch directory

### Current Problem

1. **XSEM00** tries to OPEN file 'POOL'
2. **OPEN** is NASTRAN's internal GINO I/O system (not Fortran OPEN)
3. GINO OPEN looks for existing file or creates new one
4. **Without OSCAR data**, OPEN may:
   - Block waiting for file
   - Try to create file but fail
   - Wait for uninitialized data structures

### Why POOL Doesn't Exist

The POOL file should be created by the **PREFACE** module, which:
1. Reads input file (d01000a.inp)
2. Parses executive control deck (SOL 6, APP DISP)
3. Loads rigid format file (rf/rf6.dat or similar)
4. Compiles DMAP instructions
5. Writes POOL file with execution plan

**Current Flow**:
```
nastrn.f → XSEM00 → Try to read POOL
           ↑
           └── POOL doesn't exist (never created!)
```

**Expected Flow**:
```
nastrn.f → XSEM00 → PREFACE → Read input
                              ↓
                           Compile DMAP
                              ↓
                           Create POOL
                              ↓
                           Execute modules from POOL
```

---

## Missing Components

### 1. Input File Reading (PREFACE)

**Expected**: XSEM00 should call PREFACE to:
- Read NASTRAN input file from stdin
- Parse executive control statements
- Load rigid format
- Compile DMAP

**Actual**: XSEM00 assumes POOL already exists

**Evidence**: XSEM00 code jumps directly to:
```fortran
CALL OPEN(*270,POOL,DATABF(IBUF1),2)  ! Line 87
CALL READ(*280,*40,POOL,INOSCR,200,1,ERRFLG)  ! Line 91
```

No input parsing or DMAP compilation visible in XSEM00

### 2. Rigid Format Files

**Location Expected**: `/path/to/rf/` directory
**Contents**: Rigid format DMAP source files
- `rf1.dat` - Static analysis
- `rf2.dat` - Normal modes
- `rf3.dat` - Buckling
- `rf6.dat` - Aeroelastic response (referenced in d01000a.inp)
- etc.

**Current Status**:
```bash
ls /mnt/developer/git/aecs4u.it/calculix.BAK/NASTRAN-95/rf/
```
Needs verification if files exist

### 3. PREFACE Module

**Search Results**:
```bash
grep -ri "SUBROUTINE PREFACE" src/ mis/ mds/
```
(Results TBD - need to verify if PREFACE exists)

**Alternative Names**:
- PREFACE
- SEMINI (SEM initialize)
- RFLOAD (Rigid Format Load)
- INPUTT (Input module)

---

## Proposed Solutions

### Option 1: Fix Missing PREFACE Call (Recommended)

**Steps**:
1. Locate PREFACE module or equivalent (may be called differently)
2. Modify XSEM00 to call PREFACE before attempting to read POOL
3. Ensure PREFACE reads from stdin
4. Ensure PREFACE creates POOL file

**Code Change Example**:
```fortran
SUBROUTINE XSEM00
  ! ... initialization ...

  ! NEW: Call PREFACE to process input and create POOL
  CALL PREFACE (INPUT_FILE_UNIT, POOL_FILE)

  ! EXISTING: Now POOL exists, can proceed
  CALL OPEN(*270,POOL,DATABF(IBUF1),2)
  CALL READ(*280,*40,POOL,INOSCR,200,1,ERRFLG)
  ! ...
END SUBROUTINE
```

**Estimated Effort**: 8-16 hours (locate, integrate, test)

---

### Option 2: Standalone PREFACE Execution

**Approach**:
1. Create separate `nastpre` executable that runs PREFACE only
2. `nastpre` reads input, creates POOL file, exits
3. `nastran` executable reads existing POOL file

**Workflow**:
```bash
nastpre < input.inp          # Creates POOL file
nastran < POOL > output.txt  # Reads POOL, executes
```

**Pros**:
- Separation of concerns
- Easier to debug
- Matches some NASTRAN implementations

**Cons**:
- Requires two executables
- More complex workflow

**Estimated Effort**: 12-20 hours

---

### Option 3: Bypass POOL (Simplified Test)

**Approach**: Create minimal DMAP directly without POOL file

**Steps**:
1. Hardcode simple DMAP sequence in XSEM00
2. Skip POOL read/write
3. Execute minimal module sequence

**Example**:
```fortran
! Skip POOL reading
! GO TO 87  ! Skip OPEN/READ

! Hardcoded simple execution
CALL GP1      ! Grid point processing
CALL TA1      ! Stiffness assembly
CALL SSG1     ! Static solution
CALL OFP      ! Output formatting
STOP
```

**Pros**:
- Quickest way to test if modules work
- Validates compilation

**Cons**:
- Not general solution
- Only works for one problem type
- Not true NASTRAN behavior

**Estimated Effort**: 4-8 hours

---

## Investigation Tasks

### Task 1: Locate PREFACE or Equivalent

**Commands**:
```bash
# Search for PREFACE
grep -ri "SUBROUTINE PREFACE" src/ mis/ mds/ bd/
grep -ri "SUBROUTINE SEMINI" src/ mis/ mds/
grep -ri "SUBROUTINE RFLOAD" src/ mis/ mds/

# Search for functions that read input file
grep -ri "READ.*EXECUTIVE" src/ mis/ mds/
grep -ri "NASTRAN INPUT" src/ mis/ mds/
grep -ri "BEGIN BULK" src/ mis/ mds/

# Search for POOL creation
grep -ri "WRITE.*POOL" src/ mis/ mds/
grep -ri "CREATE.*POOL" src/ mis/ mds/
```

**Expected Findings**:
- PREFACE subroutine in 800-1500 lines
- Calls to INPUTT, XREAD, or similar parsers
- POOL file creation code

---

### Task 2: Verify Rigid Format Files

**Commands**:
```bash
cd /path/to/NASTRAN-95
find . -name "rf*.dat" -o -name "rf*.f" -o -name "rigid*"
ls -lh rf/
cat rf/rf1.dat | head -50  # Check format
```

**Expected Findings**:
- rf1.dat through rf16.dat (or subset)
- DMAP source format (Fortran or custom)
- Readable by PREFACE module

---

### Task 3: Trace GINO OPEN Call

**Approach**: Add debug output to GINO OPEN

**File**: `src/system/io/gopen.f` (or similar)

**Modification**:
```fortran
SUBROUTINE OPEN (...)
  PRINT *, 'DEBUG: OPEN called for file:', FILNAM
  PRINT *, 'DEBUG: Buffer address:', BUF
  ! ... existing code ...
  PRINT *, 'DEBUG: OPEN complete, status:', ISTAT
END SUBROUTINE
```

**Recompile and Run**:
```bash
cd build && cmake --build . 2>&1 | grep -E "(error|warning)" | tail -10
cd ../examples/input
export DBMEM=12000000 OCMEM=2000000 RFDIR=../../rf DIRCTY=/tmp LOGNM=test.log
timeout 5 ../../build/bin/nastran < d01000a.inp 2>&1 | head -20
```

**Expected Output**:
```
DEBUG: OPEN called for file: POOL
DEBUG: Buffer address: 1234567
[hangs or continues]
```

This reveals exactly where execution stops

---

### Task 4: Check for Input Reader

**Search for stdin reading**:
```bash
grep -ri "READ.*\*" src/system/ | grep -i input
grep -ri "READ.*5," src/system/  # Unit 5 = stdin
grep -ri "GFREAD" src/system/
```

**Check for bulk data parser**:
```bash
grep -ri "BEGIN BULK" src/
grep -ri "ENDDATA" src/
```

---

## Workaround: Manual OSCAR Creation

If PREFACE is missing, we can manually create a minimal OSCAR/POOL file:

### OSCAR Format (Simplified)

```
Record 1: Header
  - Module name (4H format)
  - Parameter count
  - Flags

Record 2-N: Parameters
  - Data values
  - Constants

Record N+1: Next module
  ...
```

### Minimal OSCAR for Static Analysis

```fortran
! Pseudo-code for minimal OSCAR
MODULE GP1    ! Grid point processor
MODULE TA1    ! Element stiffness
MODULE SSG1   ! Static solution
MODULE OFP    ! Output
EXIT
```

### Python Script to Generate OSCAR

```python
#!/usr/bin/env python3
"""Generate minimal NASTRAN OSCAR file for testing"""

import struct

def write_module_entry(f, name, params=[]):
    """Write OSCAR module entry"""
    # Header (32 bytes)
    f.write(struct.pack('8s', name.encode().ljust(8)))  # Module name
    f.write(struct.pack('I', len(params)))               # Param count
    f.write(struct.pack('I', 0))                         # Flags
    f.write(struct.pack('16s', b'\x00'*16))              # Reserved

    # Parameters
    for param in params:
        f.write(struct.pack('d', param))

with open('POOL.oscar', 'wb') as f:
    write_module_entry(f, 'GP1')
    write_module_entry(f, 'TA1')
    write_module_entry(f, 'SSG1')
    write_module_entry(f, 'OFP')
    write_module_entry(f, 'EXIT')

print("Created POOL.oscar")
```

**Usage**:
```bash
python3 create_oscar.py
export POOL_FILE=POOL.oscar
nastran < input.inp > output.txt
```

**Note**: Requires understanding exact OSCAR binary format from NASTRAN documentation

---

## Next Steps

### Immediate (This Week)

1. **Search for PREFACE** or input processing module
2. **Verify rigid format files** exist and format
3. **Add debug output** to GINO OPEN to confirm hang location
4. **Document OSCAR format** from manuals

### Short Term (Next Week)

1. **Implement Option 1** if PREFACE found
2. **Implement Option 3** (bypass POOL) for quick test
3. **Create manual OSCAR file** for testing
4. **Test with simple static analysis** (d01011a.inp)

### Medium Term (2-3 Weeks)

1. **Full PREFACE integration** if missing
2. **Rigid format loader** if needed
3. **Complete input parser** chain
4. **End-to-end test** of all 200+ demo problems

---

## Success Criteria

### Phase 1: Basic Execution
- [ ] NASTRAN reads input from stdin without hanging
- [ ] PREFACE processes executive control deck
- [ ] POOL file created successfully
- [ ] XSEM00 reads POOL without error
- [ ] Program reaches first module (GP1 or similar)

### Phase 2: Simple Analysis
- [ ] d01000a.inp runs to completion (DMAP listing)
- [ ] d01011a.inp runs to completion (static analysis)
- [ ] Output file generated
- [ ] Log file shows execution trace

### Phase 3: Full Functionality
- [ ] All rigid formats functional (RF1-RF16)
- [ ] All 200+ demo problems run successfully
- [ ] Results match reference outputs

---

## References

### NASTRAN Documentation
- **Programmer's Manual**: Section on OSCAR format, PREFACE module
- **Theoretical Manual**: DMAP language specification
- **User's Guide**: Executive control deck format

### Source Code
- `src/system/platform/nastrn.f` - Main program entry
- `src/system/database/xsem00.f` - Execution engine
- `src/system/io/` - GINO I/O system
- `mds/` - Original system files

### External Resources
- COSMIC NASTRAN source code archive
- NASTRAN User Group forums
- NASA Technical Reports on NASTRAN

---

**Last Updated**: February 9, 2026
**Status**: Investigation in progress
**Blocking Issue**: Missing POOL/OSCAR file creation
**Priority**: **P0 - CRITICAL**
