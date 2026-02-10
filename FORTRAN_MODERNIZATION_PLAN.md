# NASTRAN-95 Fortran Modernization Plan

## Overview

Plan for converting NASTRAN-95 Fortran 77 fixed-form code to modern Fortran 2008/2018 standards.

**Date:** February 10, 2026
**Status:** Planning phase

## Files Modified During Parser Integration

The following files were modified during the Rust parser bridge integration and are candidates for modernization:

### 1. Core Modified Files

| File | Size | Lines | Priority | Status |
|------|------|-------|----------|--------|
| `src/utilities/parser/rust_bridge_interface.f90` | Modern | ~140 | N/A | Already modern F2003+ ✓ |
| `src/utilities/helpers/semint.f` | 5.4 KB | ~200 | High | Pending |
| `src/utilities/output/xcsa.f` | 61 KB | ~1500 | High | Pending |
| `src/system/database/endsys.f` | 12 KB | ~442 | Medium | Pending |
| `src/utilities/output/sofut.f` | 47 KB | ~1200 | Low | Pending |
| `src/utilities/output/strscn.f` | 33 KB | ~900 | Low | Pending |
| `src/system/io/cputim.f` | 2 KB | ~60 | Low | Pending |
| `src/system/io/nastim.f` | 3 KB | ~80 | Low | Pending |

### 2. Modernization Strategy

#### Phase 1: Critical Parser Files (High Priority)
- **semint.f** → **semint.f90**
- **xcsa.f** → **xcsa.f90**

These files interface with the Rust parser bridge and should be modernized first.

#### Phase 2: Supporting Files (Medium Priority)
- **endsys.f** → **endsys.f90**

#### Phase 3: Utility Files (Low Priority)
- **sofut.f**, **strscn.f**, **cputim.f**, **nastim.f**

## Modernization Standards

### Target: Fortran 2008 with selective 2018 features

#### Core Changes

1. **Source Form**
   ```fortran
   ! Old (F77 fixed-form)
         SUBROUTINE FOO(X,Y)
         INTEGER X,Y
         X = X + 1
         END

   ! New (F90+ free-form)
   subroutine foo(x, y)
     integer, intent(inout) :: x
     integer, intent(in) :: y
     x = x + 1
   end subroutine foo
   ```

2. **Type Declarations**
   ```fortran
   ! Old
   IMPLICIT INTEGER (A-Z)
   DIMENSION ARRAY(100)
   CHARACTER*512 STR

   ! New
   use iso_fortran_env, only: int32, real64
   integer(int32) :: array(100)
   character(len=512) :: str
   ```

3. **Intent Specifications**
   ```fortran
   ! Old
   SUBROUTINE PROC(IN, OUT, INOUT)

   ! New
   subroutine proc(in, out, inout)
     integer, intent(in) :: in
     integer, intent(out) :: out
     integer, intent(inout) :: inout
   ```

4. **Common Blocks → Modules**
   ```fortran
   ! Old
   COMMON /RUSTOK/ IRUST_OK
   SAVE   /RUSTOK/

   ! New
   module rust_state
     implicit none
     integer :: irust_ok = 0
   end module rust_state
   ```

5. **Modern I/O**
   ```fortran
   ! Old
   OPEN(998, FILE='/tmp/debug.txt', STATUS='UNKNOWN')
   WRITE(998,*) 'Value=', X
   CLOSE(998)

   ! New
   open(newunit=unit_num, file='/tmp/debug.txt', &
        status='replace', action='write', iostat=ios)
   if (ios == 0) then
     write(unit_num, '(a,i0)') 'Value=', x
     close(unit_num)
   end if
   ```

6. **Array Syntax**
   ```fortran
   ! Old
   DO 10 I = 1,N
     ARRAY(I) = 0
   10 CONTINUE

   ! New
   array(1:n) = 0
   ! or
   array = 0  ! if entire array
   ```

7. **Derived Types**
   ```fortran
   ! Old
   COMMON /DATA/ X, Y, Z

   ! New
   type :: data_type
     real :: x, y, z
   end type data_type
   ```

## Example: semint.f → semint.f90

### Before (F77 Fixed-Form)
```fortran
      SUBROUTINE SEMINT (DEBUG1)
C
C     SEMINT IS THE EXECUTION MONITOR FOR THE PREFACE.
C
      USE rust_parser_interface
      INTEGER         AXIC,AXIF,OUTTAP,PLOTF,HICORE,DEBUG1
      INTEGER         RUST_IERR
      CHARACTER       UFM*23,UWM*25,UIM*29,SUBR(13)*6
      CHARACTER*512   INPFILE_STR
      TYPE(ExecControlData) :: RUST_EXEC
      TYPE(CaseControlData) :: RUST_CASE
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /RUSTOK/ IRUST_OK
      SAVE   /RUSTOK/
C...
```

### After (F90+ Free-Form)
```fortran
subroutine semint(debug1)
  !
  ! SEMINT is the execution monitor for the preface.
  ! Last modified: 2026-02-10 - Added Rust parser bridge integration
  !
  use iso_fortran_env, only: int32
  use rust_parser_interface
  use rust_state_module
  implicit none

  ! Arguments
  integer(int32), intent(in) :: debug1

  ! Local variables
  integer(int32) :: axic, axif, outtap, plotf, hicore
  integer(int32) :: rust_ierr
  character(len=23) :: ufm
  character(len=25) :: uwm
  character(len=29) :: uim
  character(len=6) :: subr(13)
  character(len=512) :: inpfile_str
  type(ExecControlData) :: rust_exec
  type(CaseControlData) :: rust_case

  ! Common blocks (to be converted to modules in Phase 2)
  common /xmssg/ ufm, uwm, uim

  ! ... rest of code ...
end subroutine semint
```

## Benefits of Modernization

### 1. Type Safety
- Explicit intent prevents accidental modification
- Compile-time checking of argument types
- Better optimization opportunities

### 2. Maintainability
- Free-form is easier to read and edit
- Self-documenting with intent specifications
- Modern IDEs provide better support

### 3. Interoperability
- Better C/C++/Rust interoperability
- Standard ISO_C_BINDING (already used in rust_bridge_interface.f90)
- Easier integration with modern libraries

### 4. Performance
- Compilers can optimize modern Fortran better
- Array operations more efficient
- Better cache utilization with derived types

### 5. Error Prevention
- `implicit none` catches typos
- Intent violations caught at compile time
- Better debugging information

## Risks and Mitigation

### Risk 1: Breaking Changes
**Risk:** Modernization might introduce bugs
**Mitigation:**
- Modernize incrementally, one file at a time
- Run full test suite after each file
- Keep F77 versions as backup
- Use git branches for modernization work

### Risk 2: Build System
**Risk:** CMake might need updates for `.f90` files
**Mitigation:**
- CMake already handles `.f90` (rust_bridge_interface.f90 works)
- Update CMakeLists.txt file by file
- Test build after each change

### Risk 3: Common Blocks
**Risk:** Converting common blocks to modules requires coordinated changes
**Mitigation:**
- Phase 1: Keep common blocks, modernize syntax only
- Phase 2: Convert common to modules systematically
- Document all common block usage first

### Risk 4: Compiler Compatibility
**Risk:** Older compilers might not support F2008
**Mitigation:**
- Require gfortran ≥ 7.0 (supports F2008)
- Document compiler requirements
- Test on target platforms

## Implementation Plan

### Immediate (Next Steps)

1. **Create rust_state_module.f90**
   ```fortran
   module rust_state_module
     implicit none
     integer, save :: irust_ok = 0
   end module rust_state_module
   ```

2. **Modernize semint.f → semint.f90**
   - Convert to free-form
   - Add explicit declarations
   - Keep common blocks for now
   - Test thoroughly

3. **Modernize xcsa.f → xcsa.f90**
   - Most impactful but largest file
   - Convert incrementally
   - Maintain backward compatibility

### Short-term (1-2 weeks)

4. **Modernize endsys.f → endsys.f90**
5. **Create comprehensive test suite**
6. **Document all common blocks**

### Long-term (1-3 months)

7. **Convert common blocks to modules**
8. **Modernize remaining modified files**
9. **Consider modernizing entire NASTRAN-95 codebase**

## Tooling

### Automated Conversion Tools

1. **FORTRAN-to-Fortran (F2F)**
   - Converts fixed to free form
   - https://github.com/IBM/fort-to-fortran

2. **fprettify**
   - Formats modern Fortran
   - `pip install fprettify`

3. **camfort**
   - Refactoring tool for Fortran
   - https://camfort.github.io/

### Manual Steps Required

- Intent specifications (tool can't infer)
- Common block → module conversion
- Derived type design
- Error handling improvements

## Testing Strategy

### Unit Tests
- Test each subroutine independently
- Compare outputs with F77 version
- Use same input data

### Integration Tests
- Run full NASTRAN test suite
- Compare results with reference outputs
- Check all 132 example files

### Performance Tests
- Benchmark modernized vs original
- Ensure no performance regression
- Document any improvements

## Documentation

### Code Documentation
- Add modern comments (! instead of C)
- Document intent of arguments
- Explain derived types

### User Documentation
- Update build instructions
- Document compiler requirements
- Provide migration guide

## Success Metrics

- [ ] All modified files converted to F90+
- [ ] Zero test failures vs F77 version
- [ ] Build time unchanged or improved
- [ ] Code review approved
- [ ] Documentation complete

## References

- **Modern Fortran Explained** (Metcalf, Reid, Cohen) - F2018 standard
- **Fortran Best Practices** - https://fortran-lang.org/learn/best_practices/
- **Fortran Standards** - https://wg5-fortran.org/
- **ISO_C_BINDING** - Used in rust_bridge_interface.f90

## Timeline

| Phase | Duration | Completion |
|-------|----------|------------|
| Planning | 1 day | ✓ Complete |
| Phase 1 (semint, xcsa) | 3-5 days | Pending |
| Phase 2 (endsys) | 2 days | Pending |
| Phase 3 (utilities) | 5 days | Pending |
| Testing & Validation | 3 days | Pending |
| **Total** | **2-3 weeks** | **0% complete** |

---

**Next Action:** Create `rust_state_module.f90` and begin Phase 1 with `semint.f` modernization.
