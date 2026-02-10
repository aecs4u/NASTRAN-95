# Fortran File Migration Checklist

Quick reference for migrating files from `mis/` to `src/utilities/`.

---

## Pre-Migration Checks

- [ ] **Check for duplicates:** Does `mis/basename.f` already exist in `src/utilities/`?
- [ ] **Check for ENTRY points:** Does file contain `ENTRY` statements that conflict with standalone files?
- [ ] **Check dependencies:** What other files does this file call/depend on?

```bash
# Find existing utilities files
find src/utilities/ -name "basename.*"

# Check for ENTRY points
grep -i "^\s*ENTRY\s" mis/basename.f

# Check what it calls
grep -i "call\|function\|subroutine" mis/basename.f | grep -v "^C"
```

---

## Migration Steps

### 1. Copy and Convert to Free-Form

```bash
# Copy to appropriate subdirectory
cp mis/basename.f src/utilities/{parser|output|helpers}/basename.f90
```

### 2. Modernize Structure (Safe Changes)

✅ **DO change:**
- File extension: `.f` → `.f90`
- Comments: `C` → `!` (column 1)
- Continuations: `&` at line end, `&` at start of next line
- Column-based layout → free-form
- Character declarations: `CHARACTER*23` → `character(len=23)`
- Comment style: `C=====` → `!` with proper indentation

❌ **DON'T change:**
- **Hollerith constants:** `4HTIME` stays `4HTIME` (NOT `4htime`)
- Hollerith character data case
- Logic or algorithm
- Variable names (unless modernizing fully)

### 3. Critical: Preserve Hollerith Case

```fortran
! WRONG - will break card matching:
data ectt / 51, &
     4htime,4h    ,0   ,   4happ ,4h    ,0

! CORRECT - preserves uppercase:
data ectt / 51, &
     4HTIME,4H    ,0   ,   4HAPP ,4H    ,0
```

**Rule:** The `h`/`H` prefix is case-insensitive, but the CHARACTER DATA must match exactly.

### 4. Move Original to Legacy

```bash
# Move original to archive
mv mis/basename.f src/legacy/mis/basename.f

# CMakeLists.txt will auto-exclude it from legacy build
```

### 5. Build and Test

```bash
cd build
cmake --build . 2>&1 | grep -i error

# Verify correct symbol is linked
nm --print-size bin/nastran | grep "basename_"
nm --print-size src/utilities/.../basename.f90.o | grep "basename_"
# Sizes should MATCH
```

### 6. Test Execution

```bash
# Run test suite
./test_rust_bridge.sh

# Or run specific test
cd examples/input
../../build/bin/nastran test_case.inp
```

---

## Common Issues

### Issue 1: Duplicate Symbols

**Symptom:**
```
multiple definition of `symbol_'; first defined here
```

**Cause:** Both `mis/file.f` and `src/utilities/file.f90` are being compiled.

**Fix:** Move `mis/file.f` to `src/legacy/mis/` (CMake auto-excludes it).

---

### Issue 2: ENTRY Point Conflicts

**Symptom:**
```
multiple definition of `finwrt_'; first defined here
```

**Cause:** `mis/onetwo.f` has `ENTRY FINWRT`, and `mis/finwrt.f` also exists.

**Fix:** Move BOTH files to `src/legacy/mis/`.

---

### Issue 3: Cards Not Recognized

**Symptom:**
```
*** USER FATAL MESSAGE  507, ILLEGAL SPECIFICATION OR FORMAT ON PRECEDING CARD.
```

**Cause:** Hollerith constants lowercased during conversion.

**Fix:** Restore ALL UPPERCASE in Hollerith character data:
```fortran
! Find all Hollerith constants
grep -n "[0-9]h" file.f90

# Compare with original
diff -u src/legacy/mis/file.f src/utilities/.../file.f90
```

---

### Issue 4: Wrong Symbol Linked

**Symptom:** Code doesn't execute, no errors at build time.

**Cause:** Linker using old version from `mis/`.

**Verification:**
```bash
# Check symbol sizes
nm --print-size build/bin/nastran | grep "symbol_"
nm --print-size build/src/utilities/.../file.f90.o | grep "symbol_"

# Sizes should MATCH
```

**Fix:** Ensure old file moved to `src/legacy/mis/`, rebuild clean:
```bash
rm -rf build/src/legacy/CMakeFiles/nastran_legacy.dir
cmake --build build
```

---

## Phase 1 vs Phase 2 Modernization

### Phase 1 (Conservative)
✅ Safe for initial migration
- Free-form conversion
- Comment modernization
- Character declaration updates
- **Keep:** IMPLICIT INTEGER, DIMENSION, EQUIVALENCE, DATA, GOTO

### Phase 2 (Complete)
⚠️ Requires thorough testing
- Remove IMPLICIT, add `implicit none`
- Explicit variable declarations with intent
- Modern array syntax
- Replace GOTO with structured control flow
- Convert COMMON blocks to modules

**Recommendation:** Do Phase 1 first, verify it works, then do Phase 2.

---

## Automated Tools

### Find All Duplicates
```bash
cd /mnt/developer/git/aecs4u.it/NASTRAN-95
for f in src/utilities/*/*.f90; do
  base=$(basename "$f" .f90)
  if [ -f "mis/$base.f" ]; then
    echo "DUPLICATE: $base"
  fi
done
```

### Find ENTRY Conflicts
```bash
# Find all ENTRY points in utilities
grep -rn "^\s*ENTRY\s" src/utilities/ | while read line; do
  entry=$(echo "$line" | sed 's/.*ENTRY\s*\([A-Z0-9]*\).*/\1/')
  if [ -f "mis/${entry,,}.f" ]; then
    echo "CONFLICT: $entry"
  fi
done
```

### Verify Symbol Linking
```bash
# Create verification script
cat > verify_symbols.sh << 'EOF'
#!/bin/bash
for obj in build/src/utilities/**/*.o; do
  base=$(basename "$obj" .o)
  sym="${base,,}_"
  exe_size=$(nm --print-size build/bin/nastran | grep " $sym\$" | awk '{print $2}')
  obj_size=$(nm --print-size "$obj" | grep " $sym\$" | awk '{print $2}')
  if [ "$exe_size" != "$obj_size" ]; then
    echo "MISMATCH: $base (exe:$exe_size obj:$obj_size)"
  fi
done
EOF
chmod +x verify_symbols.sh
```

---

## Reference Files

**Successfully migrated examples:**
- [xcsa.f90](../src/utilities/output/xcsa.f90) - Phase 1 (conservative)
- [semint.f90](../src/utilities/helpers/semint.f90) - Phase 2 (complete)
- [rust_state_module.f90](../src/utilities/helpers/rust_state_module.f90) - Modern module

**See also:**
- [XCSA Fix Complete Report](XCSA_FIX_COMPLETE.md)
- [Fortran Modernization Plan](FORTRAN_MODERNIZATION_PLAN.md)

---

**Last Updated:** 2026-02-10
