# NASTRAN-95 Execution Flow Analysis

## Execution Sequence from Start

```
┌─────────────────────────────────────────────────────────────┐
│ 1. NASTRN.F (Main Program Entry)                           │
│    Location: src/system/platform/nastrn.f                   │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        ├─ Read environment variables (RFDIR, DIRCTY, LOGNM, etc.)
                        ├─ Open file units (unit 3 = LOGNM = log file)
                        ├─ Initialize system variables
                        │
                        v
┌─────────────────────────────────────────────────────────────┐
│ 2. CALL SEMINT(DEBUG1)                                      │
│    Location: src/utilities/helpers/semint.f90               │
│    Purpose: Execution monitor for the preface               │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        ├─ Line 34: IRUST_OK = 0 (initialize)
                        ├─ Line 41: CALL NASCAR (read NASTRAN card)
                        ├─ Line 45: CALL DEFCOR (define open core)
                        ├─ Line 56: CALL GNFIAT (generate file tables)
                        ├─ Line 66-69: CALL TMTSIO, TMTSLP (timing)
                        │
                        ├─ Lines 73-90: **Rust Bridge Integration** ⚠️
                        │   ├─ Get INPFILE from environment
                        │   ├─ Open debug file /tmp/rust_debug.txt
                        │   ├─ Call parse_nastran_with_rust()
                        │   └─ Set IRUST_OK = 1 if successful
                        │
                        ├─ Line 96: **CALL XCSA** ← Executive control processor
                        │
                        v
┌─────────────────────────────────────────────────────────────┐
│ 3. XCSA Subroutine Entry                                    │
│    Location: src/utilities/output/xcsa.f90                  │
│    Purpose: Read and process executive control deck         │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        ├─ Lines 141-154: **Initialization Section** ⚠️
                        │   ├─ ITOP = 0, IBOT = 0, etc.
                        │   ├─ Lines 157-176: Rust Bridge (Option C)
                        │   │   ├─ IRUST_OK = 0
                        │   │   ├─ Get INPFILE from environment
                        │   │   ├─ Call parse_nastran_with_rust()
                        │   │   └─ Set IRUST_OK = 1 if successful
                        │   └─ **ISSUE: This section not executing!** ❌
                        │
                        ├─ Lines 177-211: Machine-dependent initialization
                        ├─ Lines 212-224: Card reading loop setup
                        │
                        v
┌─────────────────────────────────────────────────────────────┐
│ 4. Card Processing Loop                                     │
│    Lines 220-260: Main card reading and dispatch            │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        ├─ Line 220: ASSIGN 70 TO IRTN1
                        ├─ Line 224: CALL XREAD (read control card)
                        ├─ Line 231: WRITE to OUTTAP (echo card)
                        ├─ Line 248: CALL XRCARD (parse card)
                        │
                        ├─ Line 261-278: Dispatch based on card type
                        │   ├─ SOL card → Line 390
                        │   ├─ APP card → Line 120
                        │   ├─ DIAG card → Line 480
                        │   ├─ TIME card → Line 110
                        │   └─ Unknown → Line 690 (error)
                        │
                        v
┌─────────────────────────────────────────────────────────────┐
│ 5. Error Handling (MESSAGE 507)                            │
│    Lines 750-770: Invalid card format                       │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        ├─ Line 750-760: Error detected
                        ├─ Line 1160: MSGNUM = 507
                        │
                        ├─ Lines 1162-1167: **Rust Bridge Check** ✓
                        │   ├─ Write debug to OUTTAP
                        │   ├─ IF (IRUST_OK == 1) THEN
                        │   │   └─ GO TO 20 (skip error)
                        │   └─ ELSE continue to error message
                        │
                        ├─ Line 1168: Write error message
                        │   "USER FATAL MESSAGE 507, ILLEGAL SPECIFICATION"
                        │   "IMHERE = 520" or "IMHERE = 397"
                        │
                        └─ Return to card reading loop
```

## Key Observations

### 1. **SEMINT Rust Bridge (Lines 73-90)**
- ✅ Executes in normal flow
- ✅ File-based debug output
- ❓ Creates /tmp/rust_debug.txt
- ❓ Status: **Not verified** (file not created in tests)

### 2. **XCSA Initialization (Lines 141-176)**
- ❌ **NOT EXECUTING** despite XCSA being called
- ❌ Debug writes don't appear
- ❌ IRUST_OK initialization not happening
- 🔍 **Root cause unknown**

### 3. **XCSA Card Processing (Lines 220+)**
- ✅ **CONFIRMED EXECUTING** (MESSAGE 507 appears)
- ✅ Error handling works
- ❌ But IRUST_OK check fails (always 0)

### 4. **MESSAGE 507 Check (Lines 1162-1167)**
- ✅ Code is in place
- ❌ IRUST_OK is 0 (not 1)
- ❌ Bypass doesn't trigger

## Execution Flow Diagram

```
NASTRN → SEMINT → XCSA → Card Loop → Error Handler
   ↓        ↓       ↓         ↓            ↓
  [1]      [2]     [3]       [4]          [5]
           │       │         │            │
           │       │         │            └─ MESSAGE 507 issued
           │       │         └────────────── Cards processed ✓
           │       └────────────────────────  Init skipped? ❌
           └──────────────────────────────── Rust bridge ❓
```

## Critical Issue

**XCSA's initialization section (lines 141-176) is not executing**, but later code (line 750+) IS executing. This suggests:

### Hypothesis A: SAVE Attribute Persistence
- Variables with SAVE retain values across calls
- First call: initialization runs
- Subsequent calls: **initialization skipped**
- Evidence: Fortran allows skipping initialization on re-entry

### Hypothesis B: Computed Entry Point
- XCSA called via computed GO TO or assigned GO TO
- Entry point bypasses initialization
- Less likely (no ENTRY statements found)

### Hypothesis C: Compiler Optimization
- Dead code elimination
- Initialization deemed unreachable
- Unlikely (other code in same section executes)

## Call Frequency Analysis

XCSA appears to be called **once per run** based on:
- Executive control deck processed once
- CEND card terminates processing
- No loop around XCSA in SEMINT

But **initialization may be skipped** if:
- Variables already initialized (SAVE attribute)
- Compiler optimization
- Previous incomplete run left state

---

**Next:** Determine why XCSA initialization is skipped and implement fix.
